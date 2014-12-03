package grammar.reordering.EM

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import grammar.reordering.representation.Grammar
import grammar.reordering.representation.Probability
import grammar.reordering.representation.Probability.{LogNil, LogOne}

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

    val m = 2
    val miniBatchSize = 2
    val threads = 3
    
    val gInit = InsideOutside.initialIteration(sents zip alignments)
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
      
    val trainingData = (sents zip alignments)
    
    OnlineEM.runTraining(stoppingCriteria, grammarStorageDir, trainingData, gSplit, threads, miniBatchSize, m)
  }

}