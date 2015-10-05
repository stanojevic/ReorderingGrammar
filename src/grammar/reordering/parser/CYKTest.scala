package grammar.reordering.parser

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import grammar.reordering.representation.Grammar
import grammar.reordering.representation.POSseq
import grammar.reordering.EM.InsideOutside
import java.io.PrintWriter
import grammar.reordering.EM.GrammarSplitter
import grammar.reordering.alignment.Preprocessing

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
    val tags = sents.map{ sent =>
      val words = sent.split(" +").toList
      words.map{word =>
        val tag = "tag_"+word
        Map(tag -> 1.0)
      } // stupid trivial tag
    }
      
    val trainingData = (sents, alignments, tags).zipped.asInstanceOf[List[(String, String, POSseq)]]
    
    val batchSize = 1
    val threads = 2
    val lambda = 0.1
    val flattenInChart = true
    
    val attachLeft = true
    val attachRight = true
    val attachTop = true
    val attachBottom = true
    val canonicalOnly = false
    val rightBranching = false
    
    val gInit = InsideOutside.initialIteration(trainingData, attachLeft, attachRight, attachTop, attachBottom, canonicalOnly, rightBranching)

    var g:Grammar = GrammarSplitter.split(gInit, threads)
    
    val randomness = 0.0
    val kBestHardEM = 0
    
    for(i <- 1 to 10){
      val (expectedCounts, likelihood) = InsideOutside.expectation(trainingData, g, batchSize, threads, randomness, kBestHardEM, attachLeft, attachRight, attachTop, attachBottom, canonicalOnly, rightBranching)
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
    val tags = sents.map{ sent =>
      val words = sent.split(" +").toList
      words.map{word =>
        val tag = "tag_"+word
        Map(tag -> 1.0)
      } // stupid trivial tag
    }
    
    val attachLeft = true
    val attachRight = true
    val attachTop = true
    val attachBottom = true
    val canonicalOnly = false
    val rightBranching = false
      
    val trainingData = Preprocessing.zip3(sents, alignments, tags)

    val batchSize = 1
    val threads = 2
    val lambda = 0.1
    val flattenInChart = true
    
    val gInit = InsideOutside.initialIteration(trainingData, attachLeft, attachRight, attachTop, attachBottom, canonicalOnly, rightBranching)
    var g:Grammar = GrammarSplitter.split(gInit, threads)

    val randomness = 0.0
    val kBestHardEM = 0
    
    for(i <- 1 to 10){
      val (expectedCounts, likelihood) = InsideOutside.expectation(trainingData, g, batchSize, threads, randomness, kBestHardEM, attachLeft, attachRight, attachTop, attachBottom, canonicalOnly, rightBranching)
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