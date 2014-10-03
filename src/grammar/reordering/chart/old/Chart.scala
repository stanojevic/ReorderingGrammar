package grammar.reordering.chart.old

import grammar.reordering.chart.old.Rule.Term

/**
 * @param n the number of words
 */
class Chart [A <: Edge] (n:Int) {
  
  private val cells : Array[Array[Map[Rule.Node, Set[Edge]]]] = Array.ofDim(n, n)
  for(i <- 0 until n){
    for(j <- 0 until n){
      cells(i)(j) = Map[Rule.Node, Set[Edge]]()
    }
  }

  private val backgroundCells : Array[Array[Map[Rule.Node, Set[Edge]]]] = Array.ofDim(n, n)
  for(i <- 0 until n){
    for(j <- 0 until n){
      backgroundCells(i)(j) = Map[Rule.Node, Set[Edge]]().withDefaultValue(Set())
    }
  }
  
  def addTerminal(i:Int, term:Term) : Unit = {
    cells(i)(i) = cells(i)(i)+(term -> Set())
  }
  
  def add(i:Int, j:Int, edge:A) : Unit = {
    val lhs = edge.rule.lhs
    val currentSet = cells(i)(j).getOrElse(lhs, Set())
    val newSet = currentSet + edge
    cells(i)(j) = cells(i)(j)+(lhs -> newSet)
  }

  def apply(i:Int)(j:Int) : Map[Rule.Node, Set[Edge]] = cells(i)(j)
  
  def backgroundList(i:Int)(j:Int) : Map[Rule.Node, Set[Edge]] = backgroundList(i)(j)
  
}

object Chart{
  
  def loadFromFile[A <: Edge](fn:String) : Chart[A] = {
    null
  }
  
}