package grammar.reordering.parser.metric

import grammar.reordering.parser.SimpleTreeNode

trait LinearMetricFunction[A] extends MetricFunction {

  def extractFeatureCountsSys(sys:SimpleTreeNode) : Map[A, Double]
  
  def extractFeatureCountsRef(ref:SimpleTreeNode) : Map[A, Double]
  
  def scoreWithFeatures(features:Map[A, Double], expFeatures:scala.collection.Map[A, Double]) : Double = {
    var totalScore = 0.0
    
    for((featureName, count) <- features){
      totalScore += count*expFeatures(featureName)
    }
    
    totalScore
  }

}