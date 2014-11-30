package grammar.reordering.EM

import grammar.reordering.representation.Probability
import grammar.reordering.representation.Probability.{LogNil}
import grammar.reordering.representation.Grammar
import grammar.reordering.representation.Rule
import java.io.File

object BatchEM {

  def runTraining(
                 stoppingCriteria : (Probability, Probability, Int) => Boolean,
                 grammarStoragePrefix : String,
                 trainingData:List[(String, String)],
                 initG:Grammar,
                 threads:Int,
                 threadBatchSize:Int
                    ) : Unit = {
    var initCounts = Map[Rule, Double]()
    for(rule <- initG.allRules){
      initCounts += rule -> 1.0
    }
    var currentCounts = initCounts
    
    var prevLikelihood = LogNil
    var currentLikelihood = LogNil // unimporant initialization
    var it = 0
    var currentG = initG

    do{
      val result = iteration(trainingData, currentG, threadBatchSize, threads)
      currentG = result._1
      currentLikelihood = result._2
      
      currentG.save(grammarStoragePrefix+"_grammar_"+it)
      System.err.println(s"\nGrammar $it $currentLikelihood\n")
      
      it += 1
    }while( ! stoppingCriteria(prevLikelihood, currentLikelihood, it))
  }

  private def iteration(
                 trainingData:List[(String, String)],
                 g:Grammar,
                 batchSize:Int,
                 threads:Int
                    ) : (Grammar, Probability) = {
    val (expectedCounts, likelihood) = InsideOutside.expectation(trainingData, g, batchSize, threads)
    val newGrammar = InsideOutside.maximization(g, expectedCounts)
    (newGrammar, likelihood)
  }

}
