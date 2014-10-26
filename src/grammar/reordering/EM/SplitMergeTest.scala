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
import java.io.PrintWriter

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
    val sents = List(
        "Nekakva recenica koja nema mnogo smisla",
        "Nekakva recenica koja nema mnogo smisla",
        "Nekakva recenica koja nema mnogo smisla",
        "Nekakva recenica koja nema mnogo smisla"
        )
    val alignments = List(
        "1-1 2-0 3-0 4-0",
        "1-1 2-0 3-1 4-0",
        "1-1 2-0 3-2 4-0",
        "1-3 2-0 3-3 4-1"
        )

    val batchSize = 1
    val parallel = true
    
    val gInit = InsideOutside.initialIteration(sents, alignments)
    val gSplitInit = SplitMerge.split(gInit, Map().withDefaultValue(1.0))
    val gSplitInit2 = SplitMerge.split(gSplitInit, Map().withDefaultValue(1.0))
    val gSplitInit3 = SplitMerge.split(gSplitInit2, Map().withDefaultValue(1.0))
    val gSplitInit4 = SplitMerge.split(gSplitInit3, Map().withDefaultValue(1.0))
    
    var grammars = List[Grammar](gSplitInit4)
    var likelihoods = List[Double](Math.log(0.0))
    var mergeLikelihoods = List[Map[(NonTerm, NonTerm, NonTerm), Double]]()
    
    var iteration = 1
    var difference = 1.0
    
    // val limit = 0.000000000000000000000000001
    val limit = Double.NegativeInfinity 

    while(difference>=limit && iteration < 30){
      println(s"Starting training iteration $iteration")
      val (expectedCounts, mergeLikelihood, likelihood) = InsideOutside.expectation(sents, alignments, grammars.head, batchSize, parallel)
      val gNew = InsideOutside.maximization(grammars.head, expectedCounts)
      
      difference = likelihood - likelihoods.head
      
      if(difference>=limit){
        grammars ::= gNew
        likelihoods ::= likelihood
        mergeLikelihoods ::= mergeLikelihood
      }else{
        println("bad stuff")
      }
      println(s"\nGrammar $iteration $likelihood\t"+Math.exp(likelihood)+"\n")
      println(s"difference $difference")
      iteration += 1
    }
    println(s"\nBest grammar "+grammars.size+" "+likelihoods.head+"\t"+Math.exp(likelihoods.head)+"\n")
    val stream = new PrintWriter("./grammar.txt")
    stream.println(grammars.head)
    if(stream != System.out)
      stream.close()

    val streamMerge = new PrintWriter("./mergeLikelihood.txt")
    for(el <- mergeLikelihoods.last){
      streamMerge.println(el)
    }
    if(streamMerge != System.out)
      streamMerge.close()
  }

}