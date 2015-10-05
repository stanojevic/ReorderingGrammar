package grammar.reordering

import grammar.reordering.parser.SimpleTreeNode
import grammar.reordering.parser.MBR
import java.io.PrintStream
import scala.io.Source
import grammar.reordering.representation.Probability

object MBRfromSample {

  private case class Config(
      samplesFile : String = "",
      inputCorpus: String = null
  )

  private val argumentParser = new scopt.OptionParser[Config]("ReorderingGrammar") {

      head("MBR from sample of ReorderingGrammar trees", "0.1")

      opt[String]('s', "samplesFile") required() action { (x, c) =>
        c.copy(samplesFile = x)
      }

      opt[String]('i', "inputCorpusFile") action { (x, c) =>
        c.copy(inputCorpus = x)
      }

      help("help") text("prints this usage text")

  }

  private def loadSamples(file: String) : List[List[SimpleTreeNode]] = {
    
    val mapa = scala.collection.mutable.Map[Int, List[SimpleTreeNode]]().withDefaultValue(List())
    
    Source.fromFile(file).getLines().foreach{ line =>
      // System.err.println("load: "+line)
      val things = line.split("\t")
      val sentId = things(0).toInt
      val prob = things(1).toDouble
      val tree = SimpleTreeNode.fromPennString(things(2))
      val realTree = SimpleTreeNode (
        tree.label,
        Probability(prob),
        tree.subTreeP,
        tree.children,
        tree.span
      )
      mapa(sentId)::=realTree
    }

    mapa.keys.toList.sorted.map{mapa(_)}
  }
  
  private def loadSents(file: String) : List[List[String]] = {
    Source.fromFile(file).getLines().toList.map{_.split(" +").toList}
  }
  
  def main(args: Array[String]) : Unit = {
    argumentParser.parse(args, Config()) map { config =>
      val samples : List[List[SimpleTreeNode]]  = loadSamples(config.samplesFile)
      val sents : List[List[String]] = if(config.inputCorpus != null){
        loadSents(config.inputCorpus)
      }else{
        samples.map{_=>null}
      }

      val sentsWithSamples : List[(List[String], List[SimpleTreeNode])]  = sents zip samples

      val out = new PrintStream(System.out, true, "UTF-8");

      for((sent, sentSamples) <- sentsWithSamples){
        val (mbrResult:List[(SimpleTreeNode, Double)], featureExpectations:Map[String, Double]) =
          MBR.rerankFast(sentSamples, new grammar.reordering.parser.metric.Kendall())
        // val dummySent = if(sent == List()) null else sent
        val reorderedSent = mbrResult.head._1.yieldReorderedWithUnaligned(sent).mkString(" ").replaceAll("UNALIGNED_", "")

        out.println(reorderedSent);
      }
    }
  }

}
