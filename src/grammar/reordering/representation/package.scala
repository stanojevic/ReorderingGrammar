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
    
    private var edgesContainer:scala.collection.mutable.Map[Edge, Edge] = scala.collection.mutable.Map()
    
    def edges = edgesContainer.valuesIterator
    
    def addEdge(edge:Edge) : Unit = {
      edgesContainer += edge -> edge
    }
    
    def removeEdge(edge:Edge) : Unit = {
      edgesContainer.remove(edge)
    }
    
    def getEdgeProb(rule : Rule, splits:List[Int]) : Option[Probability] = {
      if(edgesContainer.isEmpty){
        None
      }else{
        val randomEdge = edgesContainer.keys.head
        val start = randomEdge.start
        val end   = randomEdge.end
        val searchEdge = Edge(start, end, rule, splits)
        edgesContainer.get(searchEdge) match {
          case Some(edge) => Some(edge.rule.prob)
          case None => None
        }
      }
    }
    
    // def get(edge:Edge) = edgesContainer.get(edge)
    
    override
    def clone() : NonTermSpan = {
      val copy = new NonTermSpan()
      
      for(edge <- edges){
        copy.addEdge(edge.clone())
      }
      
      copy
    }
    
    def toString(g:Grammar) : String = {
      if(edgesContainer.isEmpty){
        "empty NonTermSpan"
      }else{
        val lhsStr = g.nonTerms(edgesContainer.keys.head.rule.lhs)
        val rulesStr = edgesContainer.values.map{_.toString(g.voc, g.nonTerms)}.mkString("\n")
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
      var children = List[(Int, Int, NonTerm)]()
      var prev = start
      var currentRhs = rule.asInstanceOf[InnerRule].rhs
      var currentSplit = splits
      while(! currentSplit.isEmpty){
        val nextSplit = currentSplit.head
        children ::= (prev, nextSplit-1, currentRhs.head)
        currentRhs = currentRhs.tail
        currentSplit = currentSplit.tail
        prev = nextSplit
      }
      children ::= (prev, end, currentRhs.head)
      
      children.reverse
    }
    
    override
    def clone() : Edge = {
      Edge(start, end, rule, splits, LogNil)
    }
    
    override
    def equals(other:Any) : Boolean = other match {
      case Edge(otherStart, otherEnd, otherRule, otherSplits, otherInside) =>
        otherStart  == start  &&
        otherEnd    == end    &&
        otherRule   == rule   &&
        otherSplits == splits
      case _ =>
        false
    }
    
    override
    def hashCode() = hash
  }
  
  type Chart = Array[Array[TIntObjectHashMap[NonTermSpan]]]
  
  object ChartHelper{
    
    /**
     * @param n for sent with 10 words n = 10
     * @return chart with root node on coordinates (0,n-1)
     */
    def emptyChart(n:Int) : Chart = Array.fill(n, n)(new TIntObjectHashMap[NonTermSpan]())
    
    def copyChart(oldChart:Chart) : Chart = {
      val n = oldChart.size
      val newChart = ChartHelper.emptyChart(n)
      
      for(i <- 0 until n){
        for(j <- i until n){
          val it = oldChart(i)(j).iterator()
          while(it.hasNext()){
            it.advance()
            val lhs = it.key()
            val nts = it.value()
            newChart(i)(j).put(lhs, nts.clone())
          }
        }
      }
      
      newChart
    }
    
  }

}
