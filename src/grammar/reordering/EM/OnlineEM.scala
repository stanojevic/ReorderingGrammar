package grammar.reordering.EM

import grammar.reordering.representation.Grammar
import grammar.reordering.representation.Rule
import grammar.reordering.representation.POSseq
import grammar.reordering.representation.Chart
import grammar.reordering.representation.Probability
import grammar.reordering.representation.Probability.{LogOne, LogNil}
import java.io.File
import scala.collection.parallel.ForkJoinTaskSupport
import grammar.reordering.representation.InnerRule
import grammar.reordering.representation.PretermRule
import java.text.SimpleDateFormat
import java.util.Date
import grammar.reordering.alignment.AlignmentCanonicalParser
import grammar.reordering.alignment.AlignmentForestParserWithTags

object OnlineEM {
  
  def runTraining(
                 stoppingCriteria : (Probability, Probability, Int) => Boolean,
                 output : String,
                 trainingData:List[(String, String, POSseq)],
                 initG:Grammar,
                 firstIterNum:Int,
                 threads:Int,
                 threadBatchSize:Int,
                 onlineBatchSize:Int,
                 alphaRate:Double,
                 randomness:Double,
                 attachLeft:Boolean,
                 attachRight:Boolean,
                 attachTop:Boolean,
                 attachBottom:Boolean,
                 canonicalOnly:Boolean,
                 rightBranching:Boolean
                    ) : Unit = {
    var initCounts = Map[Rule, Double]()
    for(rule <- initG.allRules){
      initCounts += rule -> 1000.0
    }
    var currentCounts:scala.collection.Map[Rule, Double] = initCounts
    
    var prevLikelihood = LogNil
    var currentLikelihood = LogNil // unimporant initialization
    var it = firstIterNum
    var currentG = initG

    val wordCount:Double = trainingData.map{_._1.split(" +").size}.sum

    do{
      val ft = new SimpleDateFormat ("HH:mm dd.MM.yyyy")
      val date = ft.format(new Date())
      System.err.println(s"Iteration $it started at $date")
      System.err.println()

      val result = iteration(trainingData, currentG, threads, threadBatchSize, onlineBatchSize, currentCounts, alphaRate, randomness, attachLeft, attachRight, attachTop, attachBottom, canonicalOnly, rightBranching)
      currentG = result._1
      currentLikelihood = result._2
      currentCounts = result._3
      
      currentG.save(output+"/grammar_"+it, dephrased=false)
      val perplexityPerWord = Math.exp(-currentLikelihood.log/wordCount)
      System.err.println()
      System.err.println(s"Grammar $it: likelihood $currentLikelihood")
      System.err.println(s"Grammar $it: Perplexity per word $perplexityPerWord")
      System.err.println()
      
      it += 1
    }while( ! stoppingCriteria(prevLikelihood, currentLikelihood, it))
  }
  
  private def iteration(
                 trainingData : List[(String, String, POSseq)],
                 initG:Grammar,
                 threads:Int,
                 threadBatchSize:Int,
                 onlineBatchSize:Int,
                 initCounts:scala.collection.Map[Rule, Double],
                 alphaRate:Double, // must be between 0.5 (fast and instable) and 1 (slow and stable)
                 randomness:Double,
                 attachLeft:Boolean,
                 attachRight:Boolean,
                 attachTop:Boolean,
                 attachBottom:Boolean,
                 canonicalOnly:Boolean,
                 rightBranching:Boolean
                    ) : (Grammar, Probability, scala.collection.Map[Rule, Double]) = {
    
    val counts = scala.collection.mutable.Map[Rule, Double]().withDefaultValue(0.0)
    for((rule, count) <- initCounts){
      counts(rule) = count
    }
    var k = 0
    var processed = 0
    var rollingDenominator = 1.0
    var currentG = initG
    var totalProb = LogOne
    val trainingDataSize = trainingData.size
    
    trainingData.grouped(onlineBatchSize).foreach{ trainingBatch =>
      System.err.println(s"START mini expectations")
      val t1 = System.currentTimeMillis()
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
      val manyScaledExpectations:List[(Probability, scala.collection.Map[Rule, Double])] = preparedBatch.map{ miniBatch =>
        val result = miniBatch.map{ case ((sent, alignment, pos), scalingFactor) =>
          val a = AlignmentCanonicalParser.extractAlignment(alignment)
          val s = sent.split(" +").toList
          
          val alignmentParser =  new AlignmentForestParserWithTags(
              g=currentG,
              attachLeft=attachLeft,
              attachRight=attachRight,
              attachTop=attachTop,
              attachBottom=attachBottom,
              beSafeBecauseOfPruning=true,
              canonicalOnly=canonicalOnly,
              rightBranching=rightBranching)

          val chart:Chart = alignmentParser.parse(sent=s, a=a, tags=pos)
          InsideOutside.inside(chart, currentG)
          InsideOutside.outside(chart, currentG)
          
          val (unscaledExpectations, sentProb) = InsideOutside.computeSoftExpectedCountPerChart(chart, currentG, randomness)
          val chartExpectations = unscaledExpectations.mapValues(_*scalingFactor)
          
          val n = chart.size
          // val sentProb:Probability = chart(0)(n-1).get(currentG.ROOT).inside
          
          processed +=1
          if(processed % 1000 == 0){
            System.err.println(s"$processed/$trainingDataSize")
          }
          
          (sentProb, chartExpectations)
        }.toList
        result
      }.toList.flatten
      
      for(scaledExpectations <- manyScaledExpectations){
        totalProb *= scaledExpectations._1
        for((rule, count) <- scaledExpectations._2){
          counts(rule)+=count
        }
      }
      val t2 = System.currentTimeMillis()
      val period = t2 - t1
      System.err.println(s"DONE mini expectations with "+trainingBatch.size+", took "+period+" ms")
      
      val newG = InsideOutside.maximization(currentG, counts)
      
      currentG = newG
        
      k += onlineBatchSize
    }
    
    (currentG, totalProb, counts)
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
