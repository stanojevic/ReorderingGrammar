package grammar.reordering.alignment

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import grammar.reordering.representation.IntMapping
import grammar.reordering.representation.ChartHelper
import grammar.reordering.representation.Grammar
import grammar.reordering.EM.GrammarSplitter

class AlignmentForestParserWithTagsTest extends FlatSpec with ShouldMatchers{

  "parse alignment" should "not break" in {
    // val string = "1-1 2-0 3-0 4-0"
    val string = "1-1 2-0 3-1 4-0"
    val alignment = AlignmentCanonicalParser.extractAlignment(string)
    val sent = "Nekakva recenica koja nema mnogo smisla"
    
    val words = sent.split(" ").toList
    val n = words.size
    
    
    var attachLeft = false
    var attachLow  = true
    val rightBranching = false
    var label = s"left=$attachLeft low=$attachLow"
    var tree = AlignmentCanonicalParser.parse(n, alignment, attachLeft, attachLow, rightBranching)
    // AlignmentCanonicalParser.visualizeTree(tree, label)

    attachLeft = true
    attachLow  = true
    label = s"left=$attachLeft low=$attachLow"
    tree = AlignmentCanonicalParser.parse(n, alignment, attachLeft, attachLow, rightBranching)
    // AlignmentCanonicalParser.visualizeTree(tree, label)

    attachLeft = false
    attachLow  = false
    label = s"left=$attachLeft low=$attachLow"
    tree = AlignmentCanonicalParser.parse(n, alignment, attachLeft, attachLow, rightBranching)
    // AlignmentCanonicalParser.visualizeTree(tree, label)

    attachLeft = true
    attachLow  = false
    label = s"left=$attachLeft low=$attachLow"
    tree = AlignmentCanonicalParser.parse(n, alignment, attachLeft, attachLow, rightBranching)
    // AlignmentCanonicalParser.visualizeTree(tree, label)

    println("done")
    // System.in.read()

    val voc = new IntMapping()
    words.foreach(voc(_))
    
    val tags = words.map{word =>
      val tag = "tag_"+word
      Map(tag -> 1.0)
    } // stupid trivial tag
    
    // val nonTerms = AlignmentForestParser.defaultNonTermsUnmarked
    val nonTerms = AlignmentForestParserWithTags.createNonTermsMappingWithTags(tags.map{_.keys.head})
    val dummyLatent = AlignmentForestParserWithTags.createDummyLatentMappings(nonTerms)
    
    val g = new Grammar(
        rulesArg = List(),
        latentMappings = dummyLatent,
        nonTerms = nonTerms,
        voc = voc,
        dummy=true
        )
    
    val alignmentParser1 = new AlignmentForestParserWithTags(g=g, attachLeft=true, attachRight=true, attachTop=true, attachBottom=true, beSafeBecauseOfPruning=true, canonicalOnly=false, rightBranching=false)
    val chartDummy = alignmentParser1.parse(sent=words, a=alignment, tags=tags)
    
    val splitG = GrammarSplitter.split(g, threads=2)

    val alignmentParser2 = new AlignmentForestParserWithTags(g=splitG, attachLeft=true, attachRight=true, attachTop=true, attachBottom=true, beSafeBecauseOfPruning=true, canonicalOnly=false, rightBranching=false)
    val chart = alignmentParser2.parse(sent=words, a=alignment, tags=tags)
    
    // val n = chart.size
    System.out.println(ChartHelper.chartToString(chart, g))
  }
}
