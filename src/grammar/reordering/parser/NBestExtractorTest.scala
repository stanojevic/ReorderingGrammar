package grammar.reordering.parser

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import grammar.reordering.representation.Grammar
import grammar.reordering.EM.InsideOutside
import grammar.reordering.EM.SplitMerge
import java.io.PrintWriter
import grammar.reordering.EM.AlignmentCanonicalParser

class NBestExtractorTest extends FlatSpec with ShouldMatchers{

  "nbest extraction" should "not fail" in {
    val sents = List("Nekakva recenica koja nema mnogo smisla")
    
    val g:Grammar = Grammar.loadFromFile("./grammar.txt")

    var sent = sents.head.split(" +").toList
    sent = sent++sent++sent++sent++sent
    val chart = CYK.buildChart(g, sent)
    
    val delatentizedChart = CYK.deLatentizeChart(g, chart)
    
    val k = 200
    val result = NBestExtractor.extractKbest(g, delatentizedChart, k)
    var i = 1
    AlignmentCanonicalParser.visualizeTree(result.head._1, "rank="+i+"/"+result.size+" p="+result.head._2.toDouble)
    result.take(k).foreach{ weightedTree =>
      //AlignmentCanonicalParser.visualizeTree(weightedTree._1, "rank="+i+"/"+result.size+" p="+weightedTree._2.toDouble)
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