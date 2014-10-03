package grammar.reordering.representation

object ProbabilityHelper {
  
  val logNil = Math.log(0.0)
  val logOne = Math.log(1.0)
  
  /**
   * using some of the tricks from
   * http://gasstationwithoutpumps.wordpress.com/2014/05/06/sum-of-probabilities-in-log-prob-space/
   * not checking for underflowing (is that necessary?)
   */
  def logSumExp(a:Double, b:Double) : Double = {
    val res = b+Math.log1p(Math.exp(a-b))
    if(res.isNaN)
      Math.max(a, b)
    else
      res
  }
  
  def logSumExp(x:List[Double]) : Double = {
    x reduce logSumExp
  }

}