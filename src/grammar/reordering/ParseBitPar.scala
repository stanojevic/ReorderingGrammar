package grammar.reordering

import scala.io.Source
import grammar.reordering.representation.Grammar
import grammar.reordering.alignment.Preprocessing
import scala.collection.parallel.ForkJoinTaskSupport
import grammar.reordering.parser.CYK
import grammar.reordering.parser.NBestExtractor
import beer.permutation.pet.representation.TreeNode
import grammar.reordering.representation.Probability
import java.io.PrintWriter
import grammar.reordering.parser.Yield
import scala.sys.process._

object ParseBitPar {

  private case class Config(
    sentencesFN: String = "",
    grammarFN: String = "",
    kBest: Int = 1,
    outPermutedStringFN: String = null,
    outTreeFN: String = null,
    outQuasiPermFN: String = null,
    threads: Int = 1,
    bitParEx: String = "",
    nonParallelBatchSize: Int = 10)

  private val argumentParser = new scopt.OptionParser[Config]("ReorderingGrammar") {

    head("ReorderingGrammar", "0.1")

    opt[String]('s', "sentencesFile") required () action { (x, c) =>
      c.copy(sentencesFN = x)
    }

    opt[String]('g', "grammarFile") required () action { (x, c) =>
      c.copy(grammarFN = x)
    }

    opt[String]("bitpar") required () action { (x, c) =>
      c.copy(bitParEx = x)
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

    opt[Int]('t', "threads") action { (x, c) =>
      c.copy(threads = x)
    }

    opt[Int]('b', "nonParallelBatchSize") action { (x, c) =>
      c.copy(nonParallelBatchSize = x)
    }

    help("help") text ("prints this usage text")
  }

  private def loadSentences(sentsFN: String): List[String] = {
    Source.fromFile(sentsFN).getLines().toList
  }

  private def loadGrammar(grammarFN: String): Grammar = {
    Grammar.loadFromFile(grammarFN)
  }

  private def preprocess(raw: List[String], g: Grammar): List[String] = {
    Preprocessing.prepareDataForUnknownWordsGivenGrammar(raw, g)
  }

  def main(args: Array[String]): Unit = {
    argumentParser.parse(args, Config()) map { config =>

      val rawSents = loadSentences(config.sentencesFN)
      val g = loadGrammar(config.grammarFN)
      val sents = preprocess(rawSents, g)

      val tmpSentsFN = "preprocessedForParsing" // should be tmp file with delteOnExit
      val sentsPW = new PrintWriter(tmpSentsFN)
      sents.foreach{line => sentsPW.println(line)}
      sentsPW.close()
      
      val tmpSentsOutFN = "result"
      
      val tmpBitParLexiconFN = config.grammarFN+".lexicon.bitpar" // should be tmp file with delteOnExit
      val tmpBitParGrammarFN = config.grammarFN+".grammar.bitpar" // should be tmp file with delteOnExit
      System.err.println("START converting to BitPar format")
      g.saveInBitParFormat(tmpBitParLexiconFN, tmpBitParGrammarFN)
      System.err.println("DONE converting to BitPar format")
      
      
      
      val cmd = config.bitParEx +  " " +
                                 " -s " + Grammar.ROOTtoken +
                                 " -v " +
                                 tmpBitParGrammarFN + " " + tmpBitParLexiconFN + " " +
                                 tmpSentsFN // + " " + tmpSentsOutFN
      System.err.println("START BitPar with command: "+cmd)
      Process(cmd).lineStream.foreach{ line =>
        System.err.println("BLA: "+line)
      }
      System.err.println("DONE BitPar")
      
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
      
      
      if (quasiPermPW != null) quasiPermPW.close()

      if (permutedStringPW != null) permutedStringPW.close()

      if (treePW != null) treePW.close()

    } getOrElse {
      System.err.println("arguments are bad")
    }
  }
}