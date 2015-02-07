package grammar.reordering.parser.metric

import grammar.reordering.parser.SimpleTreeNode

class Kendall extends LinearMetricFunction[String] {
  
  def extractFeatureCountsSys(sys:SimpleTreeNode) : Map[String, Double] = {
    val sysPerm = sys.yieldPermutationWithUnaligned.map{ x => if(x>=0) x else x+SimpleTreeNode.UNALIGNED_SHIFT}
    val sysSkipBigrams = scala.collection.mutable.Map[String, Double]().withDefaultValue(0)

    for(i <- 0 until sysPerm.size-1){
      for(j <- i+1 until sysPerm.size){
        val bigram = sysPerm(i)+", "+sysPerm(j)
        sysSkipBigrams(bigram) += 1
      }
    }
    
    sysSkipBigrams.toMap
  }

  def extractFeatureCountsRef(ref:SimpleTreeNode) : Map[String, Double] = {
    val refPerm = ref.yieldPermutationWithUnaligned.map{ x => if(x>=0) x else x+SimpleTreeNode.UNALIGNED_SHIFT}
    val n:Double = refPerm.size
    val nPossiblePairs = n*(n-1)/2
    val refSkipBigrams = scala.collection.mutable.Map[String, Double]().withDefaultValue(0)

    for(i <- 0 until refPerm.size-1){
      for(j <- i+1 until refPerm.size){
        val bigram = refPerm(i)+", "+refPerm(j)
        refSkipBigrams(bigram) += 1/nPossiblePairs
      }
    }
    
    refSkipBigrams.toMap
  }
  
  def score(sys:SimpleTreeNode, ref:SimpleTreeNode) : Double = {
    val sysPerm = sys.yieldPermutationWithUnaligned.map{ x => if(x>=0) x else x+SimpleTreeNode.UNALIGNED_SHIFT}
    val refPerm = ref.yieldPermutationWithUnaligned.map{ x => if(x>=0) x else x+SimpleTreeNode.UNALIGNED_SHIFT}
    
    val refSkipBigrams = scala.collection.mutable.Map[(Int, Int), Int]().withDefaultValue(0)

    for(i <- 0 until refPerm.size-1){
      for(j <- i+1 until refPerm.size){
        val bigram = (refPerm(i), refPerm(j))
        refSkipBigrams(bigram) += 1
      }
    }

    val sysSkipBigramsMatch = scala.collection.mutable.Map[(Int, Int), Int]().withDefaultValue(0)
    for(i <- 0 until sysPerm.size-1){
      for(j <- i+1 until sysPerm.size){
        val bigram = (sysPerm(i), sysPerm(j))
        sysSkipBigramsMatch(bigram) = refSkipBigrams(bigram)
      }
    }
    
    val matches = sysSkipBigramsMatch.values.sum.toDouble
    val refTotal = refSkipBigrams.values.sum.toDouble
    
    matches/refTotal
  }

}
