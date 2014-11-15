package grammar.reordering.parser

import grammar.reordering.representation.Chart
import grammar.reordering.representation.Grammar
import grammar.reordering.representation.ChartHelper
import grammar.reordering.representation.`package`.PretermRule
import grammar.reordering.representation.`package`.NonTermSpan
import grammar.reordering.representation.`package`.NonTerm
import grammar.reordering.representation.`package`.Edge
import grammar.reordering.representation.`package`.Rule
import grammar.reordering.representation.`package`.InnerRule
import grammar.reordering.EM.InsideOutside
import grammar.reordering.representation.Probability
import grammar.reordering.representation.Probability.{sum, LogNil}

object CYK {

  case class DotEdge(afterDot:List[NonTerm], rule:Rule, splits:List[Int])
  
  type DotChart = Array[Array[List[DotEdge]]]
  
  def buildChart(g:Grammar, sent:List[String]) : Chart = {
    val n = sent.size
    
    val chart = ChartHelper.emptyChart(n)
    val dotChart:DotChart = Array.fill(n, n)(List())
    
    /// preterminal stuff ///
    for(i <- 0 to n-1){
      val word = sent(i)
      
      ////// step 1 //////
      val pretermRules = if(g.voc.allStrings contains sent(i)){
        g.pretermRules.filter{ case ((lhs, ruleWord), rule) =>
          ruleWord == g.voc(word)
        }.values
      }else{
        g.pretermRules.filter{ case ((lhs, ruleWord), rule) =>
          ruleWord == g.unknown
        }.values
      }
      for(rule @ PretermRule(lhs, word, p) <- pretermRules){
        if(!(chart(i)(i) contains lhs))
          chart(i)(i) += lhs -> new NonTermSpan()
        chart(i)(i)(lhs).addEdge(Edge(i, i, rule, List()))
      }
      
      ////// step 2 //////
      
      for(rule @ InnerRule(lhs, rhs, p) <- g.innerRules.values){
        if(chart(i)(i) contains rhs.head){
          dotChart(i)(i) ::= DotEdge(rhs.tail, rule, List(i+1))
        }
      }
    }
    
    /// inner stuff ///
    for(span <- 2 to n){
      for(i <- 0 until n-span+1){
        val j = i + span - 1
        
        ////// step 1 /////
        for(split <- i+1 to j){
          for(DotEdge(incompleteStack, rule @ InnerRule(lhs, rhs, p), splits) <- dotChart(i)(split-1)){
            if(chart(split)(j) contains incompleteStack.head){
              if(incompleteStack.tail.isEmpty){
                /// rule is complete
                /// add it to the real chart
                if(!(chart(i)(j) contains lhs)){
                  chart(i)(j) += lhs -> new NonTermSpan()
                }
                chart(i)(j)(lhs).addEdge(Edge(i, j, rule, splits.reverse))
              }else{
                /// rule is incomplete
                /// add it to the dot chart
                dotChart(i)(j) ::= DotEdge(incompleteStack.tail, rule, (j+1)::splits)
              }
            }
          }
        }
        
        ////// step 2 /////
        for(rule @ InnerRule(lhs, rhs, p) <- g.innerRules.values){
          if(chart(i)(j) contains rhs.head){
            if(rhs.tail.isEmpty){
              // unary rule
              if(!(chart(i)(j) contains lhs)){
                chart(i)(j) += lhs -> new NonTermSpan()
              }
              chart(i)(j)(lhs).addEdge(Edge(i, j, rule, List()))
            }else{
              // non-unary rule
              dotChart(i)(j) ::= DotEdge(rhs.tail, rule, List(j+1))
            }
          }
        }
      }
    }

    chart
  }
  
  def deLatentizeChart(g:Grammar, latentChart:Chart) : Chart = {
    val n = latentChart.size
    val chart = ChartHelper.emptyChart(n)
    
    InsideOutside.inside(latentChart, g)
    InsideOutside.outside(latentChart, g)
    val sentProb:Probability = latentChart(0)(n-1)(g.ROOT).inside
    
    //Handle PretermRules
    
    for(i <- 0 until n){
      latentChart(i)(i).values.flatMap{_.edges}.groupBy{ edge =>
        removeLatentNotations(g, edge.rule.lhs)
      }.map{ case (newLhs, edges) =>
        val r:Probability = sum(edges.map{ edge =>
          edge.rule.prob * latentChart(i)(i)(edge.rule.lhs).outside
        }.toList)
        Edge(i, i, PretermRule(newLhs, edges.head.rule.asInstanceOf[PretermRule].word, r/sentProb), List())
      }.foreach{ newEdge =>
        chart(i)(i) += newEdge.rule.lhs -> new NonTermSpan()
        chart(i)(i)(newEdge.rule.lhs).addEdge(newEdge)
      }
      
    }
    
    //Handle InnerRules

    for(span <- 2 to n){
      for(i <- 0 until n-span+1){
        val j = i + span - 1
        
        val newLhss:Set[NonTerm] = latentChart(i)(j).keySet.map{removeLatentNotations(g, _)}
        newLhss.foreach{ lhs =>
          chart(i)(j) += lhs -> new NonTermSpan()
        }
        
        val groupedEdges = latentChart(i)(j).values.flatMap{_.edges}.groupBy{
          case Edge(start, end, rule:InnerRule, splits, inside) =>
            val nonLatentLhs = removeLatentNotations(g, rule.lhs)
            val nonLatentRhs = rule.rhs.map{removeLatentNotations(g, _)}
            (nonLatentLhs, nonLatentRhs, splits)
        }
        
        groupedEdges.foreach{ case ((lhs, rhs, splits), edges) =>
          val r = sum(edges.map{case Edge(_, _, rule, _, inside) => latentChart(i)(j)(rule.lhs).outside * inside}.toList)
          val newRule = InnerRule(lhs, rhs, r/sentProb)
          val newEdge = Edge(i, j, newRule, splits)
          chart(i)(j)(lhs).addEdge(newEdge)
        }
      }
    }
    
    chart
  }
  
  private def removeLatentNotations(g:Grammar, x:NonTerm) : NonTerm = {
    val originalLhs = g.reverseLatentMappings(x)
    val originalLhsStr = g.nonTerms(originalLhs)
    val moreOriginalLhsStr = if(originalLhsStr.matches("^M.C..*")){
                               originalLhsStr.substring(4)
                             }else{
                               originalLhsStr
                             }
    g.nonTerms(moreOriginalLhsStr)
  }


}
