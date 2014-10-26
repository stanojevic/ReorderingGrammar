package grammar.reordering.representation

class Probability (private val log:Double) {
  
  if(!(Probability.isZero(log) || log <= 0.0)){
    throw new Exception(s"LogProbability cannot be $log")
  }
  
  def toDouble():Double = Probability.eexp(log)
  
  def +(yProb:Probability) : Probability = {
    
    val x = this.log
    val y = yProb.log
    
    val z = if(Probability.isZero(x)){
      y
    }else if(Probability.isZero(y)){
      x
    }else if(x > y){
      x+Probability.eln(1+Math.exp(y - x))
    }else{
      y+Probability.eln(1+Math.exp(x - y))
    }
    
    if(z > 0 && z < Probability.tolerance){
      new Probability(0.0)
    }else{
      new Probability(z)
    }
  }
  
  def *(yProb:Probability) : Probability = {
    if(Probability.isZero(this.log) || Probability.isZero(yProb.log)){
      new Probability(Probability.LogZero)
    }else{
      new Probability(this.log + yProb.log)
    }
  }
  
  override
  def toString():String = {
    "l:"+log+" p:"+this.toDouble
  }
  
}

object Probability {
  
  def product(xs:Probability*) : Probability = {
    if(xs.size == 0){
      throw new Exception("cannot compute product of empty array")
    }else{
      xs.reduce(_*_)
    }
  }
  
  def product(xs:List[Probability]) : Probability = {
    if(xs.size == 0){
      throw new Exception("cannot compute product of empty array")
    }else{
      xs.reduce(_*_)
    }
  }
  
  def sum(xs:List[Probability]) : Probability = {
    if(xs.size == 0){
      throw new Exception("cannot compute product of empty array")
    }else{
      xs.reduce(_+_)
    }
  }

  val tolerance = 1E-8
  
  private val LogZero = Math.log(0.0)
  
  private def isZero(x:Double) : Boolean = {
    if(LogZero.isNaN){
      x.isNaN
    }else{
      x == LogZero
    }
  }

  object implicits{
    // implicit def double2Probability(x:Double) : Probability = Probability(x)
    
    // implicit def probability2double(p:Probability) : Double = eexp(p.log)
  }
  
  def apply(x:Double) : Probability = {
    if(x == 0.0){
      new Probability(LogZero)
    }else{
      new Probability(Math.log(x))
    }
  }
  
  private def eexp(logRep:Double) : Double = {
    if(isZero(logRep)){
      0.0
    }else{
      Math.exp(logRep)
    }
  }
  
  private def eln(x:Double) : Double = {
    if(x == 0.0)
      LogZero
    else if(x >= 0.0)
      Math.log(x)
    else
      throw new Exception(s"Probability cannot be $x")
  }

}
