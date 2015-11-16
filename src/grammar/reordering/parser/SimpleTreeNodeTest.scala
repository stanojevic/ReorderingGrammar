package grammar.reordering.parser

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import grammar.reordering.representation.Probability
import grammar.reordering.EM.BatchEM
import grammar.reordering.alignment.Preprocessing
import grammar.reordering.EM.InsideOutside
import grammar.reordering.EM.GrammarSplitter
import grammar.reordering.alignment.AlignmentCanonicalParser
import grammar.reordering.alignment.AlignmentForestParserWithTags
import grammar.reordering.representation.Chart
import grammar.reordering.representation.Rule
import grammar.reordering.representation.Grammar

class SimpleTreeNodeTest extends FlatSpec with ShouldMatchers{

  "dotting the tree" should "not fail" in {
    // val s = "(S (N Milos) (VP (V je) (AdjP budala)))"
    val s = "(ROOT (P21 (tag_milos Milos) (P01_32 (P01*1_32 (tag_je je)) (P01*2_32 (tag_budala budala)))))"
    val sent = "Milos je budala".split(" ").toList
    val graphLabel = "testing"
    val node = SimpleTreeNode.fromPennString(s)
    node.visualize(sent, graphLabel)
    // System.in.read()
  }
  
  "permuting the tree" should "not fail" in {
    val s = "(ROOT (P2413_324 (P21 (P21*242 (tag_Milos Milos)) (P21*32 (tag_je je))) (tag_car car) (tag_i i) (tag_. .)))"
    val node = SimpleTreeNode.fromPennString(s)
    println(node.yieldPermutationWithUnaligned)
    println(node.yieldReorderedWithoutUnaligned())
    println(node.yieldReorderedWithUnaligned())
  }
  
  "extracting rules from the tree" should "not fail" in {
    def iterationNumberStopper(l1:Probability, l2:Probability, it:Int, maxIt:Int) : Boolean = {
      it>maxIt
    }

    val sents = List(
        "Nekakva recenica koja nema mnogo smisla",
        "Nekakva koja nema recenica mnogo smisla",
        "koja nema mnogo Nekakva recenica smisla",
        "Nekakva koja nema recenica mnogo smisla"// ,
        // "Nekakva koja nema recenica mnogo smisla"//,
        //"nema mnogo recenica koja Nekakva smisla"
        )
    val tags = sents.map{ sent =>
      val words = sent.split(" +").toList
      words.map{word =>
        val tag = "tag_"+word
        Map(tag -> 1.0)
      } // stupid trivial tag
    }
    val alignments = List(
        "1-1 2-0 3-0 4-0",
        "1-1 2-0 3-1 4-0",
        "1-1 2-0 3-2 4-0",
        "1-3 2-0 3-3 4-1"
        )

    val miniBatchSize = 2
    val threads = 1
    val trainingData = Preprocessing.zip3(sents, alignments, tags)
    
    val stoppingCriteria : (Probability, Probability, Int) => Boolean = iterationNumberStopper(_, _, _, 30)
    val grammarStorageDir = "batch_EM_grammars"
      
    val hardEMtopK = 0
    val hardEMiterStart = 0
    val randomness = 0.0
    
    val attachLeft = true
    val attachRight = true
    val attachTop = true
    val attachBottom = true
    val canonicalOnly = false
    val rightBranching = false
    val maxRuleProduct = false
    val maxRuleSum     = false
    
    val gInit = InsideOutside.initialIteration(trainingData, attachLeft, attachRight, attachTop, attachBottom, canonicalOnly, rightBranching)
    val gSplit = GrammarSplitter.split(gInit, threads)
      
    val g = BatchEM.runTraining(stoppingCriteria, grammarStorageDir, trainingData, gSplit, 0, threads, miniBatchSize, randomness, hardEMtopK, hardEMiterStart, attachLeft, attachRight, attachTop, attachBottom, canonicalOnly, rightBranching, maxRuleProduct, maxRuleSum)
    
    
    val a = AlignmentCanonicalParser.extractAlignment(alignments.head)
    val s:List[String] = sents.head.split(" +").toList
        
    val alignmentParser = new AlignmentForestParserWithTags(g=g, attachLeft=attachLeft, attachRight=attachRight, attachTop=attachTop, attachBottom=attachBottom, beSafeBecauseOfPruning=true, canonicalOnly=canonicalOnly, rightBranching=false)
  
    val chart:Chart = alignmentParser.parse(sent=s, a=a, tags=trainingData.head._3)

    val k = 2
    val kBestTrees = KBestExtractor.extractKbest(g, chart, k, maxRuleProduct, maxRuleSum)
    
    val node1 = kBestTrees.head
    val node2 = kBestTrees.tail.head
    
    println("The tree 0 : "+node1)
    
    val rules1 = node1.extractRules(g)
    println("The rules in tree 0 : number = "+rules1.size)
    rules1.foreach{ rule =>
      println(rule.toString(g.voc, g.nonTerms))
    }
    
    println("The tree 1 : "+node2)
    
    val rules2 = node1.extractRules(g)
    println("The rules in tree 0 : number = "+rules2.size)
    rules2.foreach{ rule =>
      println(rule.toString(g.voc, g.nonTerms))
    }
    
    val (expectations, trees, sentProb) = InsideOutside.computeHardExpectedCountPerChart(chart, g, randomness, k, maxRuleProduct, maxRuleSum)
    println(s"sentProb: $sentProb")
    println("Hard expectations :")
    println(expectations.map{ case (rule:Rule, count:Double) =>
      "\t"+rule.toString(g.voc, g.nonTerms)+s"\tcount=$count"
    }.toList.sorted.mkString("\n"))
  }

}