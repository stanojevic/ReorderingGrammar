package grammar.reordering.parser

import grammar.reordering.representation.Probability
import grammar.reordering.representation.Probability.{LogOne, LogNil}
import grammar.reordering.representation.Probability.product
import grammar.reordering.representation.Grammar
import grammar.reordering.representation.Chart
import grammar.reordering.representation.NonTerm
import grammar.reordering.representation.InnerRule
import grammar.reordering.representation.PretermRule
import grammar.reordering.representation.Edge
import grammar.reordering.EM.InsideOutside
import scala.collection.mutable.PriorityQueue
import scala.annotation.tailrec
import grammar.reordering.representation.NonTermSpan
import gnu.trove.map.hash.TIntObjectHashMap

object KBestExtractor {
  
  type BestChart = Array[Array[TIntObjectHashMap[Array[SimpleTreeNode]]]]

  private val orderingTN = Ordering.by({n:SimpleTreeNode => n.subTreeP.toDouble})
  
  private type Vector = List[Int]
  private type SimpleTreeNodeVectorized = (SimpleTreeNode, Vector)
  private val orderingTNvectorized = Ordering.by({n:SimpleTreeNodeVectorized => n._1.subTreeP.toDouble})
  
  def extractKbest(g:Grammar, chart:Chart, k:Int) : List[SimpleTreeNode] = {
    val n = chart.size
    val bestChart : BestChart = Array.fill(n, n)(new TIntObjectHashMap[Array[SimpleTreeNode]]())
    
    // process preterms
    for(i <- 0 until n){
      val it = chart(i)(i).iterator()
      while(it.hasNext()){
        it.advance()
        val lhs = it.key()
        val nonTermSpan = it.value()
        var bestK = List[SimpleTreeNode]()
        for(edge <- nonTermSpan.edges){
          edge.rule match {
            case PretermRule(lhs, word, prob) =>
              val wordNode = SimpleTreeNode(g.voc(word), LogOne, LogOne, List(), (i, i))
              val pretermNode = SimpleTreeNode(g.nonTerms(lhs), prob, prob, List(wordNode), (i, i))
              bestK = merge(k, bestK, List(pretermNode))
            case _ =>
          }
        }
        bestChart(i)(i).put(lhs, bestK.toArray)
      }
    }
    
    // process unary on preterms
    for(i <- 0 until n){
      val it = chart(i)(i).iterator()
      while(it.hasNext()){
        it.advance()
        val nonTermSpan = it.value()
        for(edge <- nonTermSpan.edges){
          if(edge.rule.isInstanceOf[InnerRule]){
            processEdge(edge, chart, bestChart, g, k)
          }
        }
      }
    }
    
    
    for(span <- 2 to n){
      for(i <- 0 until n-span+1){
        val j = i + span - 1

        var unaryEdges = List[Edge]()
        var naryEdges  = List[Edge]()
        val it = chart(i)(j).iterator()
        while(it.hasNext()){
          it.advance()
          val nonTermSpan = it.value()
          for(edge <- nonTermSpan.edges){
            if(edge.splits.size == 0){
              unaryEdges ::= edge
            }else{
              naryEdges  ::= edge
            }
          }
        }
    
        for(edge <- naryEdges){
          processEdge(edge, chart, bestChart, g, k)
        }
        
        for(edge <- unaryEdges){
          processEdge(edge, chart, bestChart, g, k)
        }
      }
    }
    
    if(bestChart(0)(n-1).contains(g.ROOT)){
      bestChart(0)(n-1).get(g.ROOT).toList
    }else{
      List()
    }
  }
  
  private def processEdge(edge:Edge, chart:Chart, bestChart:BestChart, g:Grammar, k:Int) : Unit = {
    val start = edge.start
    val end = edge.end
    val lhs = edge.rule.lhs
    val rule = edge.rule.asInstanceOf[InnerRule]
    val arity = rule.rhs.size
    val spans = edge.children
    val candidates = spans.map{case (i, j, nt) => bestChart(i)(j).get(nt)}.toArray
    val prob = edge.rule.prob
    
    val C:PriorityQueue[SimpleTreeNodeVectorized] = PriorityQueue.empty(orderingTNvectorized)
    var enqueued = scala.collection.mutable.Set[List[Int]]()
    
    val initVec = Array.fill(arity)(0).toList


    val children:List[SimpleTreeNode] = (0 until arity).map{ childOrder =>
      candidates(childOrder)(0)
    }.toList
    val firstVecNode = constructTreeNode(g, start, end, rule, children, initVec)
    C.enqueue(firstVecNode)
    enqueued += firstVecNode._2
    
    var edgeBestK = List[SimpleTreeNode]()
    var edgeBestKsize = 0
    while( ! C.isEmpty && edgeBestKsize < k){
      val pop = C.dequeue()
      
      edgeBestK ::= pop._1
      edgeBestKsize += 1
      
      val allNeighbours = neighbours(start, end, rule, candidates, pop._2, g)
      
      allNeighbours.foreach{ neigh =>
        if(! enqueued.contains(neigh._2)){
          enqueued += neigh._2
          C.enqueue(neigh)
        }
      }
    }
    
    bestChart(start)(end).putIfAbsent(lhs, Array())
    val newBest = merge(k, bestChart(start)(end).get(lhs).toList, edgeBestK.reverse)
    bestChart(start)(end).put(lhs, newBest.toArray)
  }
  
  private def constructTreeNode(
      g:Grammar,
      i:Int,
      j:Int,
      rule:InnerRule,
      children:List[SimpleTreeNode],
      vec:Vector) : SimpleTreeNodeVectorized = {
    val node = SimpleTreeNode(
      g.nonTerms(rule.lhs),
      rule.prob,
      rule.prob*product(children.map{_.subTreeP}),
      children,
      (i, j)
    )
    (node, vec)
  }
  
  @tailrec
  private def merge(
      k:Int,
      xs:List[SimpleTreeNode],
      ys:List[SimpleTreeNode],
      kBestSize:Int = 0,
      kBest:List[SimpleTreeNode]=List()) : List[SimpleTreeNode] = {
    if(kBestSize >= k){
      kBest.reverse
    }else if(xs.isEmpty){
      kBest.reverse ++ ys.take(k-kBestSize)
    }else if(ys.isEmpty){
      kBest.reverse ++ xs.take(k-kBestSize)
    }else if(orderingTN.compare(xs.head, ys.head) > 0){
      merge(k, xs.tail, ys     , kBestSize+1, xs.head::kBest)
    }else{
      merge(k, xs     , ys.tail, kBestSize+1, ys.head::kBest)
    }
  }
  
  private def neighbours(
      start:Int,
      end:Int,
      rule:InnerRule,
      candidates:Array[Array[SimpleTreeNode]],
      vector:List[Int],
      g:Grammar) : List[SimpleTreeNodeVectorized]= {
    var res = List[SimpleTreeNodeVectorized]()
    val vec = vector.toArray
    
    for(i <- 0 until vec.size){
      vec(i) += 1
      retrieve(candidates, vec) match {
        case Some(children) =>
          res ::= constructTreeNode(g, start, end, rule, children, vec.toList)
        case None =>
      }
      vec(i) -= 1
    }
    
    res
  }
  
  private def retrieve(candidates:Array[Array[SimpleTreeNode]], vec:Array[Int]) : Option[List[SimpleTreeNode]] = {
    var retrieved = List[SimpleTreeNode]()
    
    for(i <- 0 until vec.size){
      if(candidates(i).size-1 < vec(i))
        return None
      else
        retrieved ::= candidates(i)(vec(i))
    }
    
    Some(retrieved.reverse)
  }

}
