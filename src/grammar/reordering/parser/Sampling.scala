package grammar.reordering.parser

import grammar.reordering.representation.Grammar
import grammar.reordering.representation.Chart
import grammar.reordering.EM.InsideOutside
import grammar.reordering.representation.NonTerm
import grammar.reordering.representation.Edge
import grammar.reordering.representation.Probability
import grammar.reordering.representation.Probability.{LogNil, LogOne}
import grammar.reordering.representation.PretermRule
import grammar.reordering.representation.InnerRule

object Sampling {
  
  def sampleBestFlatTrees(g:Grammar, chart:Chart, numOfSamples:Int) : List[SimpleTreeNode] = {
    val n = chart.size

    if(chart(0)(n-1).get(g.ROOT).inside == LogNil){
      InsideOutside.inside(chart, g)
    }
    
    val samples = scala.collection.mutable.Map[SimpleTreeNode, Double]().withDefaultValue(0.0)
    
    for(i <- 1 to numOfSamples){
      if(i % 1000==0){
        System.err.println(s"sample $i out of $numOfSamples")
      }
      val tree = sampleTreeRec(g, chart, 0, n-1, g.ROOT)
      val flatTree = tree.flatten
      samples(flatTree) += 1.0
    }
    val total = samples.values.sum
    var results = List[SimpleTreeNode]()
    for((k, v) <- samples){
      val prob = Probability(v/total)
      val newNode = SimpleTreeNode(k.label, prob, prob, k.children, k.span)
      results ::= newNode
    }
    
    val sortedResults = results.sortBy(_.subTreeP.toDouble).reverse
    
    System.err.println("min count: "+samples.values.min+" min prob: "+(samples.values.min/total))
    System.err.println("max count: "+samples.values.max+" max prob: "+(samples.values.max/total))
    System.err.println("best sorted prob : "+sortedResults.head.subTreeP)
    val mean = samples.values.sum.toDouble/samples.size
    val variance = samples.values.map{ x => Math.pow(x-mean, 2) }.sum / samples.size
    val stddev = Math.sqrt(variance)
    System.err.println(s"mean $mean and stddev: $stddev")
    
    sortedResults
  }
  
  
  private def sampleTreeRec(
      g:Grammar,
      chart:Chart,
      i:Int,
      j:Int,
      nt:NonTerm) : SimpleTreeNode = {
    
    val rand = scala.util.Random.nextDouble()
    val total = chart(i)(j).get(nt).inside
    var cummulative = 0.0
    var selectedEdge:Edge = null
    for(edge <- chart(i)(j).get(nt).edges){
      val edgeExp:Double = (edge.inside/total).toDouble
      if(cummulative < rand && rand < (cummulative+edgeExp)){
        selectedEdge = edge
      }
      cummulative += edgeExp
    }
    // System.err.println("selected edge "+selectedEdge.toString(g.voc, g.nonTerms))
    if(selectedEdge == null){
      selectedEdge = chart(i)(j).get(nt).edges.toList.last
    }
    
    selectedEdge.rule match {
      case PretermRule(lhs, word, prob) =>
        val wordNode = SimpleTreeNode(g.voc(word), LogOne, LogOne, List(), (i, j))
        SimpleTreeNode(g.nonTerms(lhs), LogOne, LogOne, List(wordNode), (i, j))
      case InnerRule(lhs, rhs, prob) =>
        val subSamples = selectedEdge.children.map{ case (start, end, nt) => sampleTreeRec(g, chart, start, end, nt)}
        SimpleTreeNode(g.nonTerms(lhs), LogOne, LogOne, subSamples, (i, j))
    }
  }

}
