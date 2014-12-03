package grammar.reordering.parser

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import grammar.reordering.representation.Grammar
import grammar.reordering.EM.InsideOutside
import java.io.PrintWriter
import grammar.reordering.EM.AlignmentCanonicalParser

class NBestExtractorTest extends FlatSpec with ShouldMatchers{

  "nbest extraction" should "not fail" in {
    // val sents = List("Nekakva recenica koja nema mnogo smisla")
    // val sents = List("for these reasons , we are going to vote in favour of the report .")
    val sents = List("you will be aware from the press and television that there have been a number of bomb explosions and killings in Sri Lanka .")
    
    print("loading grammar")
    var t1 = System.currentTimeMillis()
    val g:Grammar = Grammar.loadFromFile("./grammars/grammar_iter_9")
    var t2 = System.currentTimeMillis()
    var period = t2 - t1
    println(s"\ttook $period")

    var sent = sents.head.split(" +").toList
    // sent = sent++sent // ++sent++sent++sent
    val lambda = 0.03
    val flattenInChart = false

    print("building chart")
    t1 = System.currentTimeMillis()
    val chart = CYK.buildChart(g, sent, lambda)
    t2 = System.currentTimeMillis()
    period = t2 - t1
    println(s"\ttook $period")
    
    print("delatentizing chart")
    t1 = System.currentTimeMillis()
    // val delatentizedChart = CYK.deLatentizeChart(g, chart)
    t2 = System.currentTimeMillis()
    period = t2 - t1
    println(s"\ttook $period")
    
    print("rebalanceing chart")
    t1 = System.currentTimeMillis()
    val rebalancedChart = chart // CYK.rebalance(g, chart)
    t2 = System.currentTimeMillis()
    period = t2 - t1
    println(s"\ttook $period")

    val k = 1

    print("nbest extraction")
    t1 = System.currentTimeMillis()
    // val result = NBestExtractor.extractKbest(g, delatentizedChart, k)
    val result = NBestExtractor.extractKbest(g, rebalancedChart, k)
    t2 = System.currentTimeMillis()
    period = t2 - t1
    println(s"\ttook $period")

    var i = 1
    // AlignmentCanonicalParser.visualizeTree(result.head._1, "rank="+i+"/"+result.size+" p="+result.head._2.toDouble)
    result.take(k).foreach{ weightedTree =>
      AlignmentCanonicalParser.visualizeTree(weightedTree._1, sent, "rank="+i+"/"+result.size+" p="+weightedTree._2.toDouble)
      println("rank="+i+"/"+result.size+" p="+weightedTree._2.toDouble)
      i+=1
    }
    println("total = "+result.map{_._2.toDouble}.sum)
    
    val quasiPerm = Yield.treeToPermutation(result.head._1)
    val betterPerm = Yield.filterOutUnaligned(quasiPerm)
    val newStr = Yield.permuteString(quasiPerm, sent)
    println(s"quasiPerm = $quasiPerm")
    println(s"betterPerm = $betterPerm")
    println(s"newStr = $newStr")

    System.in.read()
    println("extracted "+result.size+" trees and requested "+k)
  }

}