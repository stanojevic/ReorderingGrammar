package grammar.reordering.EM

import grammar.reordering.representation.Grammar
import grammar.reordering.representation.Rule
import grammar.reordering.representation.Chart
import grammar.reordering.representation.Probability
import grammar.reordering.representation.Probability.{LogOne, LogNil}
import java.io.File
import scala.collection.parallel.ForkJoinTaskSupport
import grammar.reordering.representation.InnerRule
import grammar.reordering.representation.PretermRule

object OnlineEM {
  
  def runTraining(
                 stoppingCriteria : (Probability, Probability, Int) => Boolean,
                 grammarStoragePrefix : String,
                 trainingData:List[(String, String)],
                 initG:Grammar,
                 threads:Int,
                 threadBatchSize:Int,
                 onlineBatchSize:Int,
                 alphaRate:Double = 0.6
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
      val result = iteration(trainingData, currentG, threads, threadBatchSize, onlineBatchSize, currentCounts, alphaRate)
      currentG = result._1
      currentLikelihood = result._2
      currentCounts = result._3
      
      currentG.save(grammarStoragePrefix+"_grammar_"+it)
      println(s"\nGrammar $it $currentLikelihood\n")
      
      it += 1
    }while( ! stoppingCriteria(prevLikelihood, currentLikelihood, it))
  }
  
  private def iteration(
                 trainingData : List[(String, String)],
                 initG:Grammar,
                 threads:Int,
                 threadBatchSize:Int,
                 onlineBatchSize:Int,
                 initCounts:Map[Rule, Double],
                 alphaRate:Double // must be between 0.5 (fast and instable) and 1 (slow and stable)
                    ) : (Grammar, Probability, Map[Rule, Double]) = {
    
    val counts = scala.collection.mutable.Map[Rule, Double]().withDefaultValue(0.0)
    for((rule, count) <- initCounts){
      counts(rule) = count
    }
    var k = 0
    var processed = 0
    var rollingDenominator = 1.0
    var currentG = initG
    var totalProb = LogOne
    
    trainingData.grouped(onlineBatchSize).foreach{ trainingBatch =>
      val nks = (k until k+onlineBatchSize).map{ k => Math.pow(k+2, -alphaRate)}.toList

      val sf:(Double, List[Double]) = computeScalingFactors(rollingDenominator, nks)

      rollingDenominator = sf._1
      val scalingFactors = sf._2
      
      val preparedBatch = if(threads>1){
                            val l = (trainingBatch zip scalingFactors).grouped(threadBatchSize).toList.par
                            l.tasksupport = 
                              new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(threads))
                            l
                          }else{
                            (trainingBatch zip scalingFactors).grouped(threadBatchSize).toList
                          }
      val manyScaledExpectations:List[(Probability, Map[Rule, Double])] = preparedBatch.map{ miniBatch =>
        miniBatch.map{ case ((sent, alignment), scalingFactor) =>
          val a = AlignmentCanonicalParser.extractAlignment(alignment)
          val s = sent.split(" +").toList
          
          val chart:Chart = AlignmentForestParser.parse(s, a, currentG)
          InsideOutside.inside(chart, currentG)
          InsideOutside.outside(chart, currentG)
          val chartExpectations = InsideOutside.computeExpectedCountPerChart(chart, currentG).mapValues(_*scalingFactor)
          for(count <- chartExpectations.values){
            if(count.isNaN){
              print("nan")
            }
          }
          
          val n = chart.size
          val sentProb:Probability = chart(0)(n-1).get(currentG.ROOT).inside
          
          processed +=1
          if(processed % 10 == 0){
            System.err.println(processed)
          }
          
          (sentProb, chartExpectations)
        }.toList
      }.toList.flatten
      
      for(scaledExpectations <- manyScaledExpectations){
        totalProb *= scaledExpectations._1
        for((rule, count) <- scaledExpectations._2){
          if(count.isNaN){
            print("nan")
          }
          counts(rule)+=count
        }
      }
      
      val newG = InsideOutside.maximization(currentG, counts)
      
      currentG = newG

        
      k += onlineBatchSize
    }
    
    (currentG, totalProb, counts.toMap)
  }

  private def computeScalingFactors(oldPrevDenominator:Double, nks:List[Double]) : (Double, List[Double]) = {
    var prevDenominator = oldPrevDenominator
    var scalingFactors = List[Double]()
    for(nk <- nks){
      scalingFactors ::= nk/(prevDenominator*(1-nk))
      prevDenominator = prevDenominator*(1-nk)
    }
    (prevDenominator, scalingFactors.reverse)
  }

}
