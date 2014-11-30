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
    val threads = 2
    val lambda = 0.1
    val flattenInChart = true
    
    val gInit = InsideOutside.initialIteration(sents zip alignments)
    val gSplitInit = SplitMerge.split(gInit, Map().withDefaultValue(1.0))
    val gSplitInit2 = SplitMerge.split(gSplitInit, Map().withDefaultValue(1.0))
    val gSplitInit3 = SplitMerge.split(gSplitInit2, Map().withDefaultValue(1.0))
    val gSplitInit4 = SplitMerge.split(gSplitInit3, Map().withDefaultValue(1.0))

    var g:Grammar = gSplitInit4
    
    val trainingData = (sents zip alignments)

    for(i <- 1 to 10){
      val (expectedCounts, likelihood) = InsideOutside.expectation(trainingData, g, batchSize, threads)
      g = InsideOutside.maximization(g, expectedCounts)
      println(s"iteration $i $likelihood")
    }


    val sent = sents.head.split(" +").toList
    val chart = CYK.buildChart(g, sent, lambda)
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
    val threads = 2
    val lambda = 0.1
    val flattenInChart = true
    
    val gInit = InsideOutside.initialIteration(sents zip alignments)
    val gSplitInit = SplitMerge.split(gInit, Map().withDefaultValue(1.0))
    val gSplitInit2 = SplitMerge.split(gSplitInit, Map().withDefaultValue(1.0))
    val gSplitInit3 = SplitMerge.split(gSplitInit2, Map().withDefaultValue(1.0))
    val gSplitInit4 = SplitMerge.split(gSplitInit3, Map().withDefaultValue(1.0))

    var g:Grammar = gSplitInit4
    
    val trainingData = (sents zip alignments)

    for(i <- 1 to 10){
      val (expectedCounts, likelihood) = InsideOutside.expectation(trainingData, g, batchSize, threads)
      g = InsideOutside.maximization(g, expectedCounts)
      println(s"iteration $i $likelihood")
    }

    var sent = sents.head.split(" +").toList
    sent = sent++sent++sent++sent++sent
    val chart = CYK.buildChart(g, sent, lambda)
    // TODO is the chart right?
    println("Didn't crash :)")
    
    val pw = new PrintWriter("output")
    
    val it = chart(0)(sent.size-1).iterator()
    while(it.hasNext()){
      it.advance()
      val nonTermSpan = it.value()
      pw.println(nonTermSpan.toString(g))
    }
    pw.close
    
    
    val delatentizedChart = CYK.deLatentizeChart(g, chart)
    
    val pw2 = new PrintWriter("outputDeLatent")
    
    val itDelatentized = delatentizedChart(0)(sent.size-1).iterator()
    while(itDelatentized.hasNext()){
      itDelatentized.advance()
      val nonTermSpan = itDelatentized.value()
      pw2.println(nonTermSpan.toString(g))
    }
    pw2.println()
    pw2.println()
    pw2.println()
    val itDelatentized2 = delatentizedChart(0)(1).iterator()
    while(itDelatentized2.hasNext()){
      itDelatentized2.advance()
      val nonTermSpan = itDelatentized2.value()
      pw2.println(nonTermSpan.toString(g))
    }
    pw2.close
  }
  

}