package grammar.reordering.representation

import gnu.trove.map.hash.TIntObjectHashMap
import gnu.trove.map.hash.TObjectIntHashMap
import scala.collection.JavaConversions

class IntMapping {
  
  override
  def clone() : IntMapping = {
    val copy = new IntMapping()
    
    val it = voc.iterator()
    while(it.hasNext()){
      it.advance()
      val str = it.key()
      val int = it.value()
      copy.voc.put(str, int)
      copy.inverseVoc.put(int, str)
    }
    
    copy
  }
  
  private var locked = false
  
  def lock():Unit = {
    locked = true
  }
  
  def unlock():Unit = {
    locked = false
  }
  
  override
  def toString() : String = {
    voc.toString
  }
  
  private var maxInt = 0
  
  private val voc = new TObjectIntHashMap[String]()

  private val inverseVoc = new TIntObjectHashMap[String]()

  // private var voc = Map[String, Int]()

  // private var inverseVoc = Map[Int, String]()
  
  def size() : Int = {
    inverseVoc.size
  }
  
  /**
   * not thread safe
   */
  def apply(word:String) : Int = {
    if(! (voc contains word)){
      if(locked){
        throw new Exception(s"modifying IntMapping with $word while locked")
      }
      voc        .put( word   , maxInt )
      inverseVoc .put( maxInt , word   )
      maxInt += 1
    }
    voc.get(word)
  }
  
  def apply(index:Int) : String = inverseVoc.get(index)
  
  def allStrings() : Set[String] = JavaConversions.asScalaSet(voc.keySet).toSet
  def allInts()    : Set[Int]    = inverseVoc.keys.toSet
  
  def contains(index:Int  ) : Boolean = inverseVoc contains index
  def contains(word:String) : Boolean =        voc contains word
  
}
