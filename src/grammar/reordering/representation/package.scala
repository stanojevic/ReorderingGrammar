package grammar.reordering

package object representation {
  
  type NonTerm = Int
  type Word    = Int
  
  sealed abstract class Rule(val lhs:NonTerm, val prob:Double) {
    val logProb = Math.log(prob)
    def toString(voc:IntMapping, nonTerms:IntMapping) : String
  }
  case class PretermRule(leftHandSide : NonTerm,  word:Word, p:Double = 0.1) extends Rule(leftHandSide, p) {
    def toString(voc:IntMapping, nonTerms:IntMapping) : String = {
      val lhsStr = nonTerms(lhs)
      val rhsStr = voc(word)
      s"$lhsStr -> '$rhsStr' | $prob"
    }
  }
  case class InnerRule(leftHandSide : NonTerm,  rhs:List[NonTerm], p:Double = 0.1) extends Rule(leftHandSide, p) {
    def toString(voc:IntMapping, nonTerms:IntMapping) : String = {
      val lhsStr = nonTerms(lhs)
      val rhsStr = rhs.map{nonTerms(_)}.mkString(" ")
      s"$lhsStr -> $rhsStr | $prob"
    }
  }
  
  case class NonTermSpan(var edges:Set[Edge], var inside:Double = 0.0, var outside:Double = 0.0)
  case class Edge(start:Int, end:Int, rule:Rule, splits:List[Int], var inside:Double = 0.0) {
    def toString(voc:IntMapping, nonTerms:IntMapping) : String = {
      s"[$start $end] splits=$splits " + rule.toString(voc, nonTerms)
    }
  }
  
  type Chart = Array[Array[Map[NonTerm, NonTermSpan]]]
  
  object ChartHelper{
    
    /**
     * @param n for sent with 10 words n = 10
     * @return chart with root node on coordinates (0,n-1)
     */
    def emptyChart(n:Int, nonTerms:IntMapping) : Chart = {
      val chart:Chart = Array.ofDim(n, n)

      for(i <- 0 until n){
        for(j <- 0 until n){
          chart(i)(j) = Map[NonTerm, NonTermSpan]()
          //chart(i)(j) = nonTerms.allInts.map{ nonTerm => (nonTerm, NonTermSpan(i, j, nonTerm, Set(), 0.0, 0.0))}.toMap
        }
      }
      chart
    }
    
  }

}