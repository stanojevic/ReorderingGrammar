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
import scala.collection.mutable.PriorityQueue
import scala.annotation.tailrec
import grammar.reordering.representation.NonTermSpan
import gnu.trove.map.hash.TIntObjectHashMap
import grammar.reordering.representation.`package`.PretermRule

object NBestExtractor {

  private type WeightedTreeNode = (TreeNode, Probability)
  private val orderingWTN = Ordering.by({n:WeightedTreeNode => n._2.toDouble})
  
  private type Vector = List[Int]
  private type WeightedTreeNodeVectorized = (WeightedTreeNode, Vector)
  private val orderingWTNV = Ordering.by({n:WeightedTreeNodeVectorized => n._1._2.toDouble})
  
  def extractKbest(g:Grammar, chart:Chart, k:Int) : List[WeightedTreeNode] = {
    val n = chart.size
    val bestChart : Array[Array[TIntObjectHashMap[Array[WeightedTreeNode]]]] =
      Array.fill(n, n)(new TIntObjectHashMap[Array[WeightedTreeNode]]())
    
    for(i <- 0 until n){
      val it = chart(i)(i).iterator()
      while(it.hasNext()){
        it.advance()
        val lhs = it.key()
        val nonTermSpan = it.value()
        val el = if(g.nonTerms(lhs).endsWith("N")) -1 else 1
        var bestK = List[WeightedTreeNode]()
        for(edge <- nonTermSpan.edges){
          val prob = edge.rule.prob
          bestK = merge(k, bestK, List((TreeTerm(i, el), prob)))
        }
        bestChart(i)(i).put(lhs, bestK.toArray)
      }
    }
    
    for(span <- 2 to n){
      for(i <- 0 until n-span+1){
        val j = i + span - 1

        var unaryEdgeAndNTS = List[(Edge, NonTermSpan)]()
        var naryEdgeAndNTS  = List[(Edge, NonTermSpan)]()
        val it = chart(i)(j).iterator()
        while(it.hasNext()){
          it.advance()
          val nonTermSpan = it.value()
          for(edge <- nonTermSpan.edges){
            if(edge.splits.size == 0){
              unaryEdgeAndNTS ::= (edge, nonTermSpan)
            }else{
              naryEdgeAndNTS  ::= (edge, nonTermSpan)
            }
          }
        }
    
        
        for(processUnary <- List(false, true)){
          val edgeAndNTStoProcess = if(processUnary) unaryEdgeAndNTS else naryEdgeAndNTS
          // var bestK = List[WeightedTreeNode]()
          for((edge, nonTermSpan) <- edgeAndNTStoProcess){
            val lhs = edge.rule.lhs
            val operator = g.permutationMappings(lhs)
            val rule = edge.rule.asInstanceOf[InnerRule]
            val a = rule.rhs.size
            val spans = edge.children
            val candidates = spans.map{case (start, end, nt) => bestChart(start)(end).get(nt)}.toArray
            val prob = edge.rule.prob
            
            val C:PriorityQueue[WeightedTreeNodeVectorized] = PriorityQueue.empty(orderingWTNV)
            var enqueued = scala.collection.mutable.Set[List[Int]]()

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
            
            bestChart(i)(j).putIfAbsent(lhs, Array())
            val newBest = merge(k, bestChart(i)(j).get(lhs).toList, edgeBestK.reverse)
            bestChart(i)(j).put(lhs, newBest.toArray)
          }
        }
      }
    }
    
    if(bestChart(0)(n-1).contains(g.ROOT)){
      bestChart(0)(n-1).get(g.ROOT).toList
    }else{
      List()
    }
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

}
