package grammar.reordering.parser

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import grammar.reordering.representation.Grammar
import grammar.reordering.EM.InsideOutside
import grammar.reordering.EM.SplitMerge
import java.io.PrintWriter

class CYKTest extends FlatSpec with ShouldMatchers{

  "filling chart" should "not fail" in {
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

    var g:Grammar = gSplitInit4

    for(i <- 1 to 10){
      val (expectedCounts, mergeLikelihood, likelihood) = InsideOutside.expectation(sents, alignments, g, batchSize, parallel)
      g = InsideOutside.maximization(g, expectedCounts)
      println(s"iteration $i $likelihood")
    }


    val sent = sents.head.split(" +").toList
    val chart = CYK.buildChart(g, sent)
    // TODO is the chart right?
    println("Didn't crash :)")
    
//    val pw = new PrintWriter("output")
//    
//    for(nonTermSpan <- chart(0)(sent.size-1).values){
//      pw.println("AAA")
//      pw.println(nonTermSpan.toString(g))
//    }
//    pw.close
  }
  
  "delatentizing chart" should "not fail" in {
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

    var g:Grammar = gSplitInit4

    for(i <- 1 to 10){
      val (expectedCounts, mergeLikelihood, likelihood) = InsideOutside.expectation(sents, alignments, g, batchSize, parallel)
      g = InsideOutside.maximization(g, expectedCounts)
      println(s"iteration $i $likelihood")
    }

    var sent = sents.head.split(" +").toList
    sent = sent++sent++sent++sent++sent
    val chart = CYK.buildChart(g, sent)
    // TODO is the chart right?
    println("Didn't crash :)")
    
    val pw = new PrintWriter("output")
    
    for(nonTermSpan <- chart(0)(sent.size-1).values){
      pw.println(nonTermSpan.toString(g))
    }
    pw.close
    
    
    val delatentizedChart = CYK.deLatentizeChart(g, chart)
    
    val pw2 = new PrintWriter("outputDeLatent")
    
    for(nonTermSpan <- delatentizedChart(0)(sent.size-1).values){
      pw2.println(nonTermSpan.toString(g))
    }
    pw2.println()
    pw2.println()
    pw2.println()
    for(nonTermSpan <- delatentizedChart(0)(1).values){
      pw2.println(nonTermSpan.toString(g))
    }
    pw2.close
  }
  

}