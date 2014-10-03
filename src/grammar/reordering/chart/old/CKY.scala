package grammar.reordering.chart.old

import Rule.{Node, Term, NonTerm}

object CKY {
  
  def createChart[A <: Edge] ( sentence : List[String], g:Grammar ) : Chart[A] = {
    val n = sentence.size
    
    val chart = new Chart[Edge](n)
    
    for( i <- 0 until n ){
      val word = sentence(i)
      chart.addTerminal(i, Term(word))

      if(g.lexicon contains word){
        g.unaryClosures.asInstanceOf[Set[Rule]].foreach{
          case rule @ Rule(lhs, List(Term(`word`)), prob) =>
            chart.add(i, i, new Edge(rule, List()))
          case _ =>
        }
      }
    }
    
    for( i <- 0 until n-1 ){
      for( j <- i+1 until n){
        for( split <- i until j-1 ){
          //handle binary (i, split) (split+1, j)
          g.nAryRules.foreach{
            case rule @ Rule(lhs, rhs @ List(_, _), prob) =>
              
          }
        }
        //handle unary (i, j)
      }
    }
    
    null
  }
  
  def extractKBest[A <: Edge](chart:Chart[A]) : List[Set[A]] = {
    null
  }

}