package grammar.reordering.chart.old

case class Rule (val lhs:Rule.NonTerm, val rhs:List[Rule.Node], val prob:Double)

class UnaryClosureRule (
    left:Rule.NonTerm,
    right:Rule.NonTerm,
    prob:Double,
    val chain:List[Rule]  ) extends Rule(left, List(right), prob)

object Rule{
  sealed class Node(label : String)
  case class NonTerm(label : String) extends Node(label)
  case class Term(label : String) extends Node(label)
  
}
