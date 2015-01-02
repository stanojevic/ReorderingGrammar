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

object Parse {

  private case class Config(
    sentencesFN: String = "",
    grammarFN: String = "",
    kBest: Int = 1,
    lambdaPruning: Double = 0.1,
    grammarPruning: Double = 0.05,
    outPermutedStringFN: String = null,
    outTreeFN: String = null,
    outQuasiPermFN: String = null,
    threads: Int = 1,
    flushingSize: Int = 10)

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

    opt[String]("outQuasiPermFN") required () action { (x, c) =>
      c.copy(outQuasiPermFN = x)
    }

    opt[Int]('k', "kBest") action { (x, c) =>
      c.copy(kBest = x)
    }

    opt[Double]("lambdaPruning") action { (x, c) =>
      c.copy(lambdaPruning = x)
    }

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

      val lambda = config.lambdaPruning
      val k = config.kBest

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

      var processed = 0
      val toProcess = sents.size
      
      System.err.println("STARTED PARSING")

      sents.zipWithIndex.grouped(config.flushingSize).foreach { batch =>
        val parallelBatch = batch.toStream.par
        parallelBatch.tasksupport =
          new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(config.threads))
        parallelBatch.map {
          case (sentRaw: String, i: Int) =>
            val sent = sentRaw.split(" +").toList
            val t1 = System.currentTimeMillis()
            System.err.println(s"sent $i of length " + sent.size + " started")
            val chart = CYK.buildChart(g, sent, lambda)
            val delatentizedChart = chart // CYK.deLatentizeChart(g, chart)
            val result: List[SimpleTreeNode] = KBestExtractor.extractKbest(g, delatentizedChart, k)
            if(result.size == 0){
              System.err.println(s"FAILED to parse sent $i of length "+sent.size)
            }else{
              System.err.println(s"SUCCESS to parse sent $i of length "+sent.size)
              System.err.println(result.head.toPennString(sent))
            }

            if (processed % 10 == 0) {
              System.err.println(s"$processed/$toProcess")
            }
            processed += 1
            val t2 = System.currentTimeMillis()
            val period = (t2 - t1)/1000
            System.err.println(s"sent $i of length " + sent.size + s" finished in $period s")
            (result, i)
        }.toList.sortBy(_._2).foreach {
          case (trees: List[(TreeNode, Probability)], i: Int) => {
            val sent = sents(i).split(" +").toList
            trees.zipWithIndex.foreach { case (tree: SimpleTreeNode, rank: Int) => 
              val weight = tree.subTreeP
              if (quasiPermPW != null) {
                val quasiPerm = tree.yieldPermutationWithUnaligned()
                quasiPermPW.print(s"sent $i rank $rank prob $weight ||| ")
                quasiPermPW.println(quasiPerm)
              }
              if (permutedStringPW != null) {
                val permutedString = tree.yieldReorderedWithoutUnaligned(sent)
                permutedStringPW.print(s"sent $i rank $rank prob $weight ||| ")
                permutedStringPW.println(permutedString)
              }
              if (treePW != null) {
                val pennTree = tree.toPennString(sent)
                treePW.print(s"sent $i rank $rank prob $weight ||| ")
                treePW.println(pennTree)
              }
            }
          }
        }
      }

      if (quasiPermPW != null) quasiPermPW.close()

      if (permutedStringPW != null) permutedStringPW.close()

      if (treePW != null) treePW.close()

      System.err.println("DONE PARSING")

    } getOrElse {
      System.err.println("arguments are bad")
    }
  }
}
