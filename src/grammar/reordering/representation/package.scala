package grammar.reordering

import grammar.reordering.representation.ProbabilityHelper.{LogNil, LogOne}

package object representation {
  
  type NonTerm = Int
  type Word    = Int
  type Probability = Double
  type LogProbability = Double
  
  sealed abstract class Rule(val lhs:NonTerm, val prob:Probability) {
    val logProb:LogProbability = Math.log(prob)
    def toString(voc:IntMapping, nonTerms:IntMapping) : String
  }
  case class PretermRule(leftHandSide : NonTerm,  word:Word, p:Probability = 0.1) extends Rule(leftHandSide, p) {
    def toString(voc:IntMapping, nonTerms:IntMapping) : String = {
      val lhsStr = nonTerms(lhs)
      val rhsStr = voc(word)
      s"$lhsStr -> '$rhsStr' | $prob"
    }
  }
  case class InnerRule(leftHandSide : NonTerm,  rhs:List[NonTerm], p:Probability = 0.1) extends Rule(leftHandSide, p) {
    def toString(voc:IntMapping, nonTerms:IntMapping) : String = {
      val lhsStr = nonTerms(lhs)
      val rhsStr = rhs.map{nonTerms(_)}.mkString(" ")
      s"$lhsStr -> $rhsStr | $prob"
    }
  }
  
  class NonTermSpan(var inside:LogProbability = LogNil, var outside:LogProbability = LogNil){
    
    private var edgesContainer:Set[Edge] = Set()
    
    def edges() : List[Edge] = edgesContainer.toList
    
    def addEdges(someEdges:Set[Edge]) : Unit = {
      edgesContainer ++= someEdges
    }
  }
    
  object NonTermSpan{
    def apply(someEdges:Set[Edge]) : NonTermSpan = {
      val nonTermSpan = new NonTermSpan()
      nonTermSpan.addEdges(someEdges)
      nonTermSpan
    }
  }

  case class Edge(start:Int, end:Int, rule:Rule, splits:List[Int], var inside:LogProbability = LogNil) {
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
        }
      }
      chart
    }
    
  }

}