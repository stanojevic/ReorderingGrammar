package grammar.reordering.EM

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import grammar.reordering.representation.IntMapping
import grammar.reordering.representation.Rule
import grammar.reordering.representation.NonTerm
import grammar.reordering.representation.Grammar
import grammar.reordering.representation.`package`.InnerRule
import grammar.reordering.representation.`package`.PretermRule
import grammar.reordering.representation.Grammar

class SplitMergeTest extends FlatSpec with ShouldMatchers{

  "splitting" should "not break" in {
    val alignment = "1-1 2-0 3-0 4-0"
    val sent = "Nekakva recenica koja nema mnogo smisla"
    
    val g = InsideOutside.initialIteration(List(sent), List(alignment))
    val g1 = SplitMerge.split(g, Map().withDefaultValue(1.0))
    val g2 = SplitMerge.split(g1, Map().withDefaultValue(1.0))

    // println("\nGrammar 0\n"+g)
    // println("\nGrammar 1\n"+g1)
    // println("\nGrammar 2\n"+g2)
  }

  "splitting and training" should "not break" in {
    val alignment = "1-1 2-0 3-0 4-0"
    val sent = "Nekakva recenica koja nema mnogo smisla"
    val batchSize = 10
    val parallel = false
    
    val gInit = InsideOutside.initialIteration(List(sent), List(alignment))
    val gSplitInit = SplitMerge.split(gInit, Map().withDefaultValue(1.0))
    val gSplitInit2 = SplitMerge.split(gSplitInit, Map().withDefaultValue(1.0))
    
    var grammars = List[Grammar](gSplitInit2)
    
    for(i <- 1 to 15){
      val (expectedCounts, mergeLikelihood, likelihood) = InsideOutside.expectation(List(sent), List(alignment), grammars.head, batchSize, parallel)
      val gNew = InsideOutside.maximization(grammars.head, expectedCounts)
      grammars ::= gNew
      
      println(s"\nGrammar $i $likelihood\t"+Math.exp(likelihood)+"\n")
      //println(gNew)
    }
    println(grammars.head)
  }

}