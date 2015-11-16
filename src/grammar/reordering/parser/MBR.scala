package grammar.reordering.parser

import grammar.reordering.parser.metric.MetricFunction
import grammar.reordering.parser.metric.LinearMetricFunction
import grammar.reordering.representation.Probability.sum
import grammar.reordering.representation.Probability

object MBR {
  
  def rerankFast[A](listToRerank: List[SimpleTreeNode], metricFunc: LinearMetricFunction[A]) : (List[(SimpleTreeNode, Double)], Map[A, Double]) = {
    System.err.println("STARTED MBRing")
    val expectations = scala.collection.mutable.Map[A, Double]().withDefaultValue(0.0)
    val list = renormalizeProbs(listToRerank)
    
    for(tree <- list){
      val features = metricFunc.extractFeatureCountsRef(tree).withDefaultValue(0.0)
      val prob     = Math.exp(tree.subTreeScore)
      for((featureName, value) <- features){
        expectations(featureName) += prob*value
      }
    }
    
    var result = List[(SimpleTreeNode, Double)]()
    for(tree <- list){
      val features = metricFunc.extractFeatureCountsSys(tree)
      val expectedScore = metricFunc.scoreWithFeatures(features, expectations)
      result ::= (tree, expectedScore)
    }
    result = result.sortBy(_._2).reverse
    
    System.err.println("DONE MBRing")
    System.err.println("best  parse expected metric score: "+result.head._2)
    System.err.println("worst parse expected metric score: "+result.last._2)

    (result, expectations.toMap.withDefaultValue(0.0))
  }
  
  def rerankClassic(listToRerank: List[SimpleTreeNode], metricFunc: MetricFunction) : List[(SimpleTreeNode, Double)] = {
    System.err.println("STARTED MBRing")
    val list = renormalizeProbs(listToRerank)
    
    var result = List[(SimpleTreeNode, Double)]()
    
    for(sys_i <- 0 until list.size){
      var expectedScore = 0.0
      for(ref_j <- 0 until list.size){
        val metricScore = metricFunc.score(list(sys_i), list(ref_j))
        expectedScore += Math.exp(list(ref_j).subTreeScore) * metricScore
      }
      result ::= (list(sys_i), expectedScore)
    }
    result = result.sortBy(_._2).reverse
    
    System.err.println("DONE MBRing")
    System.err.println("best  parse expected metric score: "+result.head._2)
    System.err.println("worst parse expected metric score: "+result.last._2)

    result
  }
  
  private def renormalizeProbs(list: List[SimpleTreeNode]) : List[SimpleTreeNode] = {
    val total = list.map{tree => Math.exp(tree.subTreeScore)}.sum
    list.map{ tree =>
      val newP = Probability(Math.exp(tree.subTreeScore)/total)
      new SimpleTreeNode(tree.label, newP.log, newP.log, tree.children, tree.span)
    }
  }

}
