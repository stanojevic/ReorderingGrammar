package grammar.reordering.EM

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import beer.permutation.pet.representation.NonTerm
import beer.permutation.pet.representation.Term
import grammar.reordering.representation.IntMapping
import grammar.reordering.representation.Grammar

class AlignmentForestParserTest extends FlatSpec with ShouldMatchers{

  "parse alignment" should "not break" in {
    // val string = "1-1 2-0 3-0 4-0"
    val string = "1-1 2-0 3-1 4-0"
    val alignment = AlignmentCanonicalParser.extractAlignment(string)
    val sent = "Nekakva recenica koja nema mnogo smisla"
    
    val words = sent.split(" ").toList
    val n = words.size
    
    
    var attachLeft = false
    var attachLow  = true
    var label = s"left=$attachLeft low=$attachLow"
    var tree = AlignmentCanonicalParser.parse(n, alignment, attachLeft, attachLow)
    AlignmentCanonicalParser.visualizeTree(tree, label)

    attachLeft = true
    attachLow  = true
    label = s"left=$attachLeft low=$attachLow"
    tree = AlignmentCanonicalParser.parse(n, alignment, attachLeft, attachLow)
    AlignmentCanonicalParser.visualizeTree(tree, label)

    attachLeft = false
    attachLow  = false
    label = s"left=$attachLeft low=$attachLow"
    tree = AlignmentCanonicalParser.parse(n, alignment, attachLeft, attachLow)
    AlignmentCanonicalParser.visualizeTree(tree, label)

    attachLeft = true
    attachLow  = false
    label = s"left=$attachLeft low=$attachLow"
    tree = AlignmentCanonicalParser.parse(n, alignment, attachLeft, attachLow)
    AlignmentCanonicalParser.visualizeTree(tree, label)

    println("done")
    System.in.read()

    val voc = new IntMapping()
    words.foreach(voc(_))
    
    val nonTerms = AlignmentForestParser.defaultNonTermsUnmarked
    
    val g = new Grammar(
        rulesArg = List(),
        latentMappings = AlignmentForestParser.defaultLatentMappingsUnmarked,
        nonTerms = AlignmentForestParser.defaultNonTermsUnmarked,
        voc = voc,
        dummy=true
        )
    
    val chart = AlignmentForestParser.parse(words, alignment, g)
    
    // val n = chart.size
    for(span <- 1 to n){
      for(i <- 0 until n-span+1){
        val j = i + span -1
        for(lhs <- chart(i)(j).keys){
          println(s"A $lhs [$i $j] edge_size="+chart(i)(j).get(lhs).edges.size)
          for(edge <- chart(i)(j).get(lhs).edges){
            println(edge.toString(voc, nonTerms))
          }
        }
      }
    }
  }
}