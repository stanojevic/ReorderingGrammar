package grammar.reordering.representation

object ProbabilityHelper {
  
  val LogNil = Double.MinValue  // Math.log(0.0)
  val LogOne = Math.log(1.0)
  val LogInfinitecimal = Math.log(Double.MinPositiveValue)
  
  /**
   * using some of the tricks from
   * http://gasstationwithoutpumps.wordpress.com/2014/05/06/sum-of-probabilities-in-log-prob-space/
   */
  
  private def isExtreme(x:Double) = x.isNaN || x.isNegInfinity
  
  def logSubstractExpWithLog1p(first:Double, second:Double) : Double = {
    val (a, b) = if(first <= second){
      (first, second)
    }else{
      (second, first)
    }
    
    val x = a-b
    
    val log1pexp = if(x<LogInfinitecimal) Math.log1p(0.0-2.0) else Math.log1p(Math.exp(x)-2.0)
    
    val result = b+log1pexp

    if(result>0)
      throw new Exception("log prob cannot be positive")
    
    result
  }
  
  private def logSumExpWithLog1p(first:Double, second:Double) : Double = {
    val (a, b) = if(first <= second){
      (first, second)
    }else{
      (second, first)
    }
    
    val x = a-b
    
    val log1pexp = if(x<LogInfinitecimal) 0.0 else Math.log1p(Math.exp(x))
    // val log1pexp = if(x<LogInfinitecimal) 0.0 else Math.log(1+Math.exp(x))

    b+log1pexp
  }
  
  def logSumExp(x: Double*): Double = 
    logSumExp(x.toList)
  
  def logSumExp(x:List[Double]): Double = {
    if(x.isEmpty){
      LogNil
    }else{
      val betterX = x.map{el => if(isExtreme(el)) LogNil else el}

      val result = x reduce logSumExpWithLog1p
      
      if(isExtreme(result)){
        LogNil
      }else{
        if(result>0){
          x reduce logSumExpWithLog1p
          // throw new Exception("log prob cannot be positive")
          // return LogOne
        }
        result
      }
    }
  }

}
