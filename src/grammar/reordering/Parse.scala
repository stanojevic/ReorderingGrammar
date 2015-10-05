package grammar.reordering

import scala.io.Source
import grammar.reordering.representation.Grammar
import grammar.reordering.alignment.Preprocessing
import scala.collection.parallel.ForkJoinTaskSupport
import grammar.reordering.parser.CYK
import grammar.reordering.parser.KBestExtractor
import beer.permutation.pet.representation.TreeNode
import grammar.reordering.representation.Probability
import java.io.PrintWriter
import grammar.reordering.parser.Yield
import grammar.reordering.parser.SimpleTreeNode
import grammar.reordering.parser.Sampling
import grammar.reordering.parser.MBR

object Parse {

  private case class Config(
    sentencesFN: String = "",
    grammarFN: String = "",

    kToExtract  : Int     =   10000,
    kToMBR      : Int     =   10000,
    kToOutput   : Int     =       1,
    doSampling  : Boolean =    true,
    
    lambdaPruning: Double = 0.1,
    grammarPruning: Double = 0.05,
    outPermutedStringFN: String = null,
    outTreeFN: String = null,
    outQuasiPermFN: String = null,
    outExpectedKendallFN: String = null,
    threads: Int = 1,
    flushingSize: Int = 10
    )

  private val argumentParser = new scopt.OptionParser[Config]("ReorderingGrammar") {

    head("ReorderingGrammar", "0.1")

    opt[String]('s', "sentencesFile") required () action { (x, c) =>
      c.copy(sentencesFN = x)
    }

    opt[String]('g', "grammarFile") required () action { (x, c) =>
      c.copy(grammarFN = x)
    }

    opt[String]("outPermutedStringFN") required () action { (x, c) =>
      c.copy(outPermutedStringFN = x)
    }

    opt[String]("outTreeFN") required () action { (x, c) =>
      c.copy(outTreeFN = x)
    }

    opt[String]("outExpectedKendallFN") required () action { (x, c) =>
      c.copy(outExpectedKendallFN = x)
    }

    opt[String]("outQuasiPermFN") required () action { (x, c) =>
      c.copy(outQuasiPermFN = x)
    }

    opt[Int]("kToExtract") action { (x, c) =>
      c.copy(kToExtract = x)
    }

    opt[Int]("kToMBR") action { (x, c) =>
      c.copy(kToMBR = x)
    }

    opt[Int]("kToOutput") action { (x, c) =>
      c.copy(kToOutput = x)
    }
    
    opt[Boolean]("doSampling") action { (x, c) =>
      c.copy(doSampling = x)
    }

    opt[Double]("lambdaPruning") action { (x, c) =>
      c.copy(lambdaPruning = x)
    } text ("bigger lambda means more pruning; lambda = 0 means no pruning ; lambda < 0 means fixed beam of size int(-lambda)")

    opt[Double]("grammarPruning") action { (x, c) =>
      c.copy(grammarPruning = x)
    }

    opt[Int]('t', "threads") action { (x, c) =>
      c.copy(threads = x)
    }

    opt[Int]('b', "flushingSize") action { (x, c) =>
      c.copy(flushingSize = x)
    }

    help("help") text ("prints this usage text")
  }

  private def loadSentences(sentsFN: String): List[String] = {
    Source.fromFile(sentsFN).getLines().toList
  }

  private def loadGrammar(grammarFN: String, grammarPruning:Double): Grammar = {
    Grammar.loadFromFile(grammarFN, grammarPruning)
  }

  // private def preprocess(raw: List[String], g: Grammar): List[String] = {
    // Preprocessing.prepareDataForUnknownWordsGivenGrammar(raw, g)
  // }

  def main(args: Array[String]): Unit = {
    argumentParser.parse(args, Config()) map { config =>
      val startDate = java.util.Calendar.getInstance().getTime()
      System.err.println("STARTED AT "+startDate.toString())

      val lambda = config.lambdaPruning

      val sents = loadSentences(config.sentencesFN)
      val g = loadGrammar(config.grammarFN, config.grammarPruning)

      val quasiPermPW = if (config.outQuasiPermFN == null) {
        null
      } else {
        new PrintWriter(config.outQuasiPermFN)
      }

      val permutedStringPW = if (config.outPermutedStringFN == null) {
        null
      } else {
        new PrintWriter(config.outPermutedStringFN)
      }

      val treePW = if (config.outTreeFN == null) {
        null
      } else {
        new PrintWriter(config.outTreeFN)
      }

      val expectedKendallPW = if (config.outExpectedKendallFN == null) {
        null
      } else {
        new PrintWriter(config.outExpectedKendallFN)
      }

      var processed = 0
      val toProcess = sents.size
      
      System.err.println("STARTED PARSING")

      val veryBeginingTime = System.currentTimeMillis()
      var startTime = System.currentTimeMillis()
      val totalSentsToProcess = sents.size

      sents.zipWithIndex.grouped(config.flushingSize).foreach { batch =>
        val parallelBatch = batch.toStream.par
        parallelBatch.tasksupport =
          new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(math.min(config.threads, batch.size)))
        parallelBatch.map {
          case (sentRaw: String, i: Int) =>
            val sent = sentRaw.split(" +").toList
            val t1 = System.currentTimeMillis()
            System.err.println(s"sent $i of length " + sent.size + " started")

            val time_chart_start = System.currentTimeMillis()
            val chart = CYK.buildChart(g, sent, lambda)
            val time_chart_end = System.currentTimeMillis()
            val period_chart = (time_chart_end - time_chart_start)/1000

            val time_sampling_start = System.currentTimeMillis()
            val rawResult: List[SimpleTreeNode] = if(config.doSampling){
              Sampling.sampleBestFlatTrees(g, chart, config.kToExtract)
            }else{
              KBestExtractor.extractKbest(g, chart, config.kToExtract)
            }
            val time_sampling_end = System.currentTimeMillis()
            val period_sampling = (time_sampling_end - time_sampling_start)/1000

            val time_mbr_start = System.currentTimeMillis()
            val (mbrResult:List[(SimpleTreeNode, Double)], featureExpectations:Map[String, Double]) =
              MBR.rerankFast(rawResult.take(config.kToMBR), new grammar.reordering.parser.metric.Kendall())
            val time_mbr_end = System.currentTimeMillis()
            val period_mbr = (time_mbr_end - time_mbr_start)/1000

            val result = mbrResult.map{_._1}

            if(result.size == 0){
              System.err.println(s"FAILED to parse sent $i of length "+sent.size)
            }else{
              System.err.println(s"SUCCESS to parse sent $i of length "+sent.size)
              System.err.println(result.head.toPennString(sent))
            }

            processed += 1
            val t2 = System.currentTimeMillis()
            val totalSentencePeriod = (t2 - t1)/1000
            System.err.println(s"sent $i of length " + sent.size + s" finished in ${totalSentencePeriod}s" +
                s", chart ${period_chart}s, sampling ${period_sampling}s, mbr ${period_mbr}s")
            
            if(processed % 10 == 0){
              var newTime = System.currentTimeMillis()
              var period = (newTime - startTime) / 1000
              System.err.println()
              System.err.print(s"$processed/$totalSentsToProcess | last chunk processed for $period s\t")
              val pastMins = (newTime-veryBeginingTime)/60000
              if(processed != 0 && pastMins != 0){
                val futureMins:Int = ( (totalSentsToProcess.toDouble-processed)/(processed.toDouble/pastMins) ).toInt
                val partialFutureHours = futureMins/60
                val partialFutureMins = futureMins%60
                System.err.print(s"| time left $partialFutureHours h $partialFutureMins m\t")
              }
              System.err.println()
              startTime = newTime
            }
            ((result.take(config.kToOutput), featureExpectations), i)
        }.toList.sortBy(_._2).foreach {
          case ((trees: List[SimpleTreeNode], featureExpectations:Map[String, Double]), i: Int) => {
            System.err.println("flushing output")
            val sent = sents(i).split(" +").toList
            trees.zipWithIndex.foreach { case (tree: SimpleTreeNode, rank: Int) => 
              val weight = tree.subTreeP
              if (quasiPermPW != null) {
                val quasiPerm = tree.yieldPermutationWithUnaligned()
                quasiPermPW.print(s"sent $i rank $rank prob $weight ||| ")
                quasiPermPW.println(quasiPerm.mkString(" "))
                quasiPermPW.flush()
              }
              if (permutedStringPW != null) {
                val permutedString = tree.yieldReorderedWithUnaligned(sent)
                permutedStringPW.print(s"sent $i rank $rank prob $weight ||| ")
                permutedStringPW.println(permutedString.mkString(" "))
                permutedStringPW.flush()
              }
              if (treePW != null) {
                val pennTree = tree.toPennString(sent)
                treePW.print(s"sent $i rank $rank prob $weight ||| ")
                treePW.println(pennTree)
                treePW.flush()
              }
              if (expectedKendallPW != null) {
                val n = sent.size
                var stringsToOut = List[String]()
                for(i <- 0 until n-1){
                  for(j <- i+1 until n){
                    val riskA = featureExpectations(s"$i, $j")
                    stringsToOut ::= s"$i:$j:$riskA"
                    val riskB = featureExpectations(s"$j, $i")
                    stringsToOut ::= s"$j:$i:$riskB"
                  }
                }
                expectedKendallPW.println(stringsToOut.mkString(" "))
                expectedKendallPW.flush()
              }
            }
          }
        }
      }

      if (quasiPermPW != null) quasiPermPW.close()

      if (permutedStringPW != null) permutedStringPW.close()

      if (treePW != null) treePW.close()

      if (expectedKendallPW != null) expectedKendallPW.close()

      System.err.println("DONE PARSING")
      System.err.println("STARTED AT "+startDate.toString())
      System.err.println("FINISHED AT "+java.util.Calendar.getInstance().getTime().toString())

    } getOrElse {
      System.err.println("arguments are bad")
    }
  }
}
