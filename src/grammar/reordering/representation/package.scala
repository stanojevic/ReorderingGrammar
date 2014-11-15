package grammar.reordering

import gnu.trove.map.hash.TIntObjectHashMap
import grammar.reordering.representation.Probability.LogNil

package object representation {
  
  type NonTerm = Int
  type Word    = Int
  
  sealed abstract class Rule(val lhs:NonTerm, val prob:Probability) {
    def toString(voc:IntMapping, nonTerms:IntMapping) : String
  }
  case class PretermRule(leftHandSide : NonTerm,  word:Word, p:Probability = Probability(0.1)) extends Rule(leftHandSide, p) {
    def toString(voc:IntMapping, nonTerms:IntMapping) : String = {
      val lhsStr = nonTerms(lhs)
      val rhsStr = voc(word)
      s"$lhsStr -> '$rhsStr' | $prob"
    }
    
    private def withoutProb() = (lhs, word)
    
    override
    def equals(o: Any) = o match{
      case x:PretermRule => this.withoutProb() == x.withoutProb()
      case x:InnerRule   => false
    }
    
    private lazy val hash = withoutProb.hashCode()
    
    override
    def hashCode() = hash
  }
  case class InnerRule(leftHandSide : NonTerm,  rhs:List[NonTerm], p:Probability = Probability(0.1)) extends Rule(leftHandSide, p) {
    def toString(voc:IntMapping, nonTerms:IntMapping) : String = {
      val lhsStr = nonTerms(lhs)
      val rhsStr = rhs.map{nonTerms(_)}.mkString(" ")
      s"$lhsStr -> $rhsStr | $prob"
    }
    
    private def withoutProb() = (lhs, rhs)
    
    override
    def equals(o: Any) = o match{
      case x:InnerRule   => this.withoutProb() == x.withoutProb()
      case x:PretermRule => false
    }
    
    private lazy val hash = withoutProb.hashCode()
    
    override
    def hashCode() = hash
  }
  
  class NonTermSpan(var inside:Probability = LogNil, var outside:Probability = LogNil){
    
    // private var edgesContainer:Set[Edge] = Set()
    private var edgesContainer:scala.collection.mutable.Set[Edge] = scala.collection.mutable.Set()
    
    def edges() : List[Edge] = edgesContainer.toList
    
    def addEdge(edge:Edge) : Unit = {
      edgesContainer += edge
    }
    
    def toString(g:Grammar) : String = {
      if(edgesContainer.isEmpty){
        "empty NonTermSpan"
      }else{
        val lhsStr = g.nonTerms(edges().head.rule.lhs)
        val rulesStr = edges().map{_.toString(g.voc, g.nonTerms)}.mkString("\n")
        lhsStr+" in\n"+rulesStr
      }
    }
  }

  case class Edge(start:Int, end:Int, rule:Rule, splits:List[Int], var inside:Probability = LogNil) {
    def toString(voc:IntMapping, nonTerms:IntMapping) : String = {
      s"[$start $end] splits=$splits " + rule.toString(voc, nonTerms)
    }
    
    private lazy val hash = (start, end, rule, splits).hashCode()
    
    lazy val children : List[(Int, Int, NonTerm)] = {
      (childrenSpans(this) zip this.rule.asInstanceOf[InnerRule].rhs).map{ case ((x, y), z) => (x,y,z)}
    }
  
    private def childrenSpans(edge:Edge) : List[(Int, Int)] = {
      val allSplits = edge.start :: edge.splits.flatMap(split => List(split-1, split)) ++ List(edge.end)
      allSplits.sliding(2, 2).map{ case List(x, y) => (x, y)}.toList
    }
    
    override
    def hashCode() = hash
  }
  
  type Chart = Array[Array[Map[NonTerm, NonTermSpan]]]
  // type Chart = Array[Array[TIntObjectHashMap[NonTermSpan]]]
  
  object ChartHelper{
    
    /**
     * @param n for sent with 10 words n = 10
     * @return chart with root node on coordinates (0,n-1)
     */
    def emptyChart(n:Int) : Chart = Array.fill(n, n)(Map[NonTerm, NonTermSpan]())
    // def emptyChart(n:Int) : Chart = Array.fill(n, n)(new TIntObjectHashMap[NonTermSpan]())
    
  }

}
