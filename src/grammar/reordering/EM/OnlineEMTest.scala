package grammar.reordering.EM

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import grammar.reordering.representation.Grammar
import grammar.reordering.representation.POSseq
import grammar.reordering.representation.Probability
import grammar.reordering.representation.Probability.{LogNil, LogOne}
import grammar.reordering.alignment.Preprocessing

class OnlineEMTest extends FlatSpec with ShouldMatchers{

  "splitting and training" should "not break" in {
    val sents = List(
        "Nekakva recenica koja nema mnogo smisla",
        "Nekakva koja nema recenica mnogo smisla",
        "koja nema mnogo Nekakva recenica smisla",
        "Nekakva koja nema recenica mnogo smisla"// ,
        // "Nekakva koja nema recenica mnogo smisla"//,
        //"nema mnogo recenica koja Nekakva smisla"
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
      
    val trainingData = Preprocessing.zip3(sents, alignments, tags)

    val m = 2
    val miniBatchSize = 2
    val threads = 3
    
    val attachLeft = true
    val attachRight = true
    val attachTop = true
    val attachBottom = true
    val canonicalOnly = false
    val rightBranching = false
    
    val gInit = InsideOutside.initialIteration(trainingData, attachLeft, attachRight, attachTop, attachBottom, canonicalOnly, rightBranching)
    val gSplit = GrammarSplitter.split(gInit, threads)
    
    var iteration = 1
    var difference = LogOne
    
    // val limit = 0.000000000000000000000000001
    val limit = Double.NegativeInfinity 
    
    def maxDiffStopper(l1:Probability, l2:Probability, it:Int, maxDiff:Double) : Boolean = {
      l2.toDouble-l1.toDouble < maxDiff
    }

    def iterationNumberStopper(l1:Probability, l2:Probability, it:Int, maxIt:Int) : Boolean = {
      it>maxIt
    }
    
    val stoppingCriteria : (Probability, Probability, Int) => Boolean = iterationNumberStopper(_, _, _, 30)
    val grammarStorageDir = "online_EM_grammars"
      
    val randomness = 0.0
    val alpha = 0.6

    OnlineEM.runTraining(stoppingCriteria, grammarStorageDir, trainingData, gSplit, 0, threads, miniBatchSize, m, randomness, alpha, attachLeft, attachRight, attachTop, attachBottom, canonicalOnly, rightBranching)
  }

}