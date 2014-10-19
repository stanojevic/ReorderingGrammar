package grammar.reordering.representation

class IntMapping {
  
  private var locked = false
  
  def lock():Unit = {
    locked = true
  }
  
  override
  def toString() : String = {
    voc.toString
  }
  
  private var maxInt = 0

  private var voc = Map[String, Int]()

  private var inverseVoc = Map[Int, String]()
  
  /**
   * not thread safe
   */
  def apply(word:String) : Int = {
    if(! (voc contains word)){
      if(locked){
        throw new Exception(s"modifying IntMapping with $word while locked")
      }
      voc        += ( word   -> maxInt )
      inverseVoc += ( maxInt -> word   )
      maxInt += 1
    }
    voc(word)
  }
  
  def apply(index:Int) : String = inverseVoc(index)
  
  def allStrings() : Set[String] = voc.keySet
  def allInts() : Set[Int] = inverseVoc.keySet
  
  def contains(index:Int  ) : Boolean = inverseVoc contains index
  def contains(word:String) : Boolean =        voc contains word
  
}
