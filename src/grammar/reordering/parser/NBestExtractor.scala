package grammar.reordering.parser

import beer.permutation.pet.representation.{
                                                        TreeNode,
                                             Term    => TreeTerm,
                                             NonTerm => TreeNonTerm
                                           }
import grammar.reordering.representation.Probability
import grammar.reordering.representation.Probability.product
import grammar.reordering.representation.Grammar
import grammar.reordering.representation.Chart
import grammar.reordering.representation.NonTerm
import grammar.reordering.representation.InnerRule
import grammar.reordering.representation.Edge
import grammar.reordering.EM.InsideOutside
import scala.collection.immutable.SortedSet
import scala.collection.immutable.TreeSet
import scala.collection.mutable.PriorityQueue
import scala.annotation.tailrec

object NBestExtractor {

  private type WeightedTreeNode = (TreeNode, Probability)
  private val orderingWTN = Ordering.by({n:WeightedTreeNode => n._2.toDouble})
  
  private type Vector = List[Int]
  private type WeightedTreeNodeVectorized = (WeightedTreeNode, Vector)
  private val orderingWTNV = Ordering.by({n:WeightedTreeNodeVectorized => n._1._2.toDouble})
  
  def extractKbest(g:Grammar, chart:Chart, k:Int) : List[WeightedTreeNode] = {
    val n = chart.size
    val bestChart : Array[Array[Map[NonTerm, Array[WeightedTreeNode]]]] =
      Array.fill(n, n)(Map().withDefaultValue(Array()))
    
    for(i <- 0 until n){
      for((lhs, nonTermSpan) <- chart(i)(i)){
        val el = if(g.nonTerms(lhs).endsWith("N")) -1 else 1
        var bestK = List[WeightedTreeNode]()
        for(edge <- nonTermSpan.edges){
          var C:SortedSet[WeightedTreeNode] = TreeSet.empty(orderingWTN)
          val prob = edge.rule.prob
          C += ((TreeTerm(i, el), prob))
          
          bestK = merge(k, bestK, C.iterator.take(k).toList)
        }
        bestChart(i)(i) += lhs -> bestK.toArray
      }
    }
    
    for(span <- 2 to n){
      for(i <- 0 until n-span+1){
        val j = i + span - 1
        
        for((lhs, nonTermSpan) <- chart(i)(j)){
          var bestK = List[WeightedTreeNode]()
          val operator = toOperator(g.nonTerms(lhs))
          for(edge <- nonTermSpan.edges){
            val rule = edge.rule.asInstanceOf[InnerRule]
            val a = rule.rhs.size
            val spans = edge.children
            val candidates = spans.map{case (start, end, nt) => bestChart(start)(end)(nt)}.toArray
            val prob = edge.rule.prob
            
            
            val C:PriorityQueue[WeightedTreeNodeVectorized] = PriorityQueue.empty(orderingWTNV)
            var enqueued = Set[List[Int]]()

            val initVec = Array.fill(a)(0).toList
            val children:List[WeightedTreeNode] = (0 until a).map{ childOrder =>
              candidates(childOrder)(0)
              }.toList
            val firstNode = constructNode(i, j, operator, rule.prob, children, initVec)
            C.enqueue(firstNode)
            enqueued += firstNode._2
            
            var edgeBestK = List[WeightedTreeNode]()
            var edgeBestKsize = 0
            while( ! C.isEmpty  &&  edgeBestKsize < k ){
              val pop = C.dequeue()
              
              edgeBestK ::= pop._1
              edgeBestKsize += 1
              
              val allNeighbours = neighbours(i, j, operator, rule.prob, candidates, pop._2)
              
              allNeighbours.foreach{ neigh =>
                if(! enqueued.contains(neigh._2)){
                  enqueued += neigh._2
                  C.enqueue(neigh)
                }
              }
              
            }
            
            bestK = merge(k, bestK, edgeBestK.reverse)
          }
          bestChart(i)(j) += lhs -> bestK.toArray
        }
      }
    }
    
    bestChart(0)(n-1)(g.ROOT).toList
  }
  
  @tailrec
  private def merge(
      k:Int,
      xs:List[WeightedTreeNode],
      ys:List[WeightedTreeNode],
      kBestSize:Int = 0,
      kBest:List[WeightedTreeNode]=List()) : List[WeightedTreeNode] = {
    if(kBestSize >= k){
      kBest.reverse
    }else if(xs.isEmpty){
      kBest.reverse ++ ys.take(k-kBestSize)
    }else if(ys.isEmpty){
      kBest.reverse ++ xs.take(k-kBestSize)
    }else if(orderingWTN.compare(xs.head, ys.head) > 0){
      merge(k, xs.tail, ys     , kBestSize+1, xs.head::kBest)
    }else{
      merge(k, xs     , ys.tail, kBestSize+1, ys.head::kBest)
    }
  }
  
  private def constructNode(
      start:Int,
      end:Int,
      operator:List[Int],
      edgeProb:Probability,
      children:List[WeightedTreeNode],
      vector:List[Int]) : WeightedTreeNodeVectorized = {
    (
        (
            TreeNonTerm(start, end, start, end, operator, children.map{_._1}),
            edgeProb*product(children.map{_._2})
        ),
        vector
    )
  }
  
  private def neighbours(
      start:Int,
      end:Int,
      operator:List[Int],
      edgeProb:Probability,
      candidates:Array[Array[WeightedTreeNode]],
      vector:List[Int]) : List[WeightedTreeNodeVectorized]= {
    var res = List[WeightedTreeNodeVectorized]()
    val vec = vector.toArray
    
    for(i <- 0 until vec.size){
      vec(i) += 1
      retrieve(candidates, vec) match {
        case Some(children) =>
          res ::= constructNode(start, end, operator, edgeProb, children, vec.toList)
        case None =>
      }
      vec(i) -= 1
    }
    
    res
  }
  
  private def retrieve(candidates:Array[Array[WeightedTreeNode]], vec:Array[Int]) : Option[List[WeightedTreeNode]] = {
    var retrieved = List[WeightedTreeNode]()
    
    for(i <- 0 until vec.size){
      if(candidates(i).size-1 < vec(i))
        return None
      else
        retrieved ::= candidates(i)(vec(i))
    }
    
    Some(retrieved.reverse)
  }
  
  private val binaryPerm = """.*P(.)(.)$""".r
  private val fourPerm = """.*P(.)(.)(.)(.)$""".r
  private val fivePerm = """.*P(.)(.)(.)(.)(.)$""".r
  private def toOperator(s:String) : List[Int] = {
    s match{
      case Grammar.ROOTtoken            => List(0, 0)
      case binaryPerm(x1, x2)           => List(x1, x2).map{_.toInt}
      case fourPerm(x1, x2, x3, x4)     => List(x1, x2, x3, x4).map{_.toInt}
      case fivePerm(x1, x2, x3, x4, x5) => List(x1, x2, x3, x4, x5).map{_.toInt}
    }
  }

}
