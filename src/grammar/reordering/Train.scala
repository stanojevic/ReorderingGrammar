package grammar.reordering

import grammar.reordering.representation.Grammar
import scala.io.Source
import grammar.reordering.EM.InsideOutside
import grammar.reordering.EM.GrammarSplitter
import grammar.reordering.representation.Probability
import grammar.reordering.EM.BatchEM
import grammar.reordering.EM.OnlineEM
import java.io.File
import grammar.reordering.EM.Preprocessing
import grammar.reordering.EM.AlignmentCanonicalParser
import java.io.PrintWriter

object Train {
  
  private case class Config(
      sourceFN : String = "",
      alignmentFN : String = "",
      outputPrefix : String = "",
      threads: Int = 1,
      batchEM: Boolean = true,
      onlineBatchSize : Int = 1000,
      threadBatchSize : Int = 1000,
      onlineAlpha:Double = 0.6,
      initIterationFN : String = null,
      iterations : Int = 30,
      convergenceThreshold : Double = -1
  )
  
  private val argumentParser = new scopt.OptionParser[Config]("ReorderingGrammar") {

      head("ReorderingGrammar", "0.1")

      opt[String]('s', "sourceFile") required() action { (x, c) =>
        c.copy(sourceFN = x)
      }
      
      opt[String]('a', "alignmentsFile") required() action { (x, c) =>
        c.copy(alignmentFN = x)
      }
      
      opt[String]('o', "outputPrefix") required() action { (x, c) =>
        c.copy(outputPrefix = x)
      }
      
      opt[Int]('t', "threads") action { (x, c) =>
        c.copy(threads = x)
      }
      
      opt[String]('g', "initIterationFN") action { (x, c) =>
        c.copy(initIterationFN = x)
      }
      
      opt[Int]('b', "threadBatchSize") action { (x, c) =>
        c.copy(threadBatchSize = x)
      }
      
      opt[Int]('i', "iterations") action { (x, c) =>
        c.copy(iterations = x)
      }

      opt[Double]("convergenceThreshold") action { (x, c) =>
        c.copy(convergenceThreshold = x)
      }
      
      opt[Unit]("onlineEM") action { (_, c) =>
        c.copy(batchEM = false)
      }
      
      opt[Int]("onlineBatchSize") action { (x, c) =>
        c.copy(onlineBatchSize = x)
      }
      
      opt[Double]("onlineAlpha") action { (x, c) =>
        c.copy(onlineAlpha = x)
      }

      help("help") text("prints this usage text")
    }

  
  
  private def loadOrMakeInitGrammar(
      initIterationFN:String,
      trainingData:List[(String, String)],
      grammarOutputPrefix:String,
      threads:Int ) : Grammar = {
    if(initIterationFN == null){
      System.err.println("START creating init grammar")
      val initG = InsideOutside.initialIteration(trainingData)
      System.err.println("DONE creating init grammar")
      System.err.println("START splitting init grammar")
      val splittedGrammar = GrammarSplitter.split(initG, threads)
      System.err.println("DONE splitting init grammar")
      splittedGrammar.save(grammarOutputPrefix+"initGrammar")
      System.err.println("init grammar is saved")
      splittedGrammar
    }else{
      Grammar.loadFromFile(initIterationFN)
    }
  }
  
  private def createStoppingCriterion(config:Config) : (Probability, Probability, Int) => Boolean = {
    def maxDiffStopper(l1:Probability, l2:Probability, it:Int, maxDiff:Double) : Boolean = {
      l2.toDouble-l1.toDouble < maxDiff
    }

    def iterationNumberStopper(l1:Probability, l2:Probability, it:Int, maxIt:Int) : Boolean = {
      it>maxIt
    }
    
    if(config.convergenceThreshold == -1){
      iterationNumberStopper(_, _, _, config.iterations)
    }else{
      maxDiffStopper(_, _, _, config.convergenceThreshold)
    }
  }
    
  private def basicTests(config:Config) : Unit = {
    if(! new File(config.sourceFN).exists()){
      System.err.println("Source sents file "+config.sourceFN+" doesn't exist")
      System.exit(-1)
    }
    if(! new File(config.alignmentFN).exists()){
      System.err.println("Alignment file "+config.alignmentFN+" doesn't exist")
      System.exit(-1)
    }
    val storageDir = new File(config.outputPrefix)
    if( ! storageDir.isDirectory()){
      System.err.println("Output should be an existing directory")
      System.exit(-1)
    }
  }
  
  private def loadSents(file: String) : List[String] = Source.fromFile(file).getLines().toList
  
  private def filtering(oldData : List[(String, String)]) : List[(String, String)] = {
    System.err.println("STARTED filtering")
    var processed = 0
    val newData = oldData.filter{ case (sent, alignment) =>
      processed += 1
      if(processed % 10000 == 0){
        System.err.println(processed)
      }
      val a = AlignmentCanonicalParser.extractAlignment(alignment)
      val arity = Preprocessing.maxArity(a)

      arity <= 4 && Preprocessing.numAlignedWords(a) >=2
    }
    System.err.println("DONE filtering")
    System.err.println("kept "+newData.size+" out of "+oldData.size)
    
    newData
  }
  
  private def saveData(srcFN:String, alignFN:String, data:List[(String, String)]) : Unit = {
    val srcPW = new PrintWriter(srcFN)
    val alignPW = new PrintWriter(alignFN)
    for((srcSent, alignment) <- data){
      srcPW.println(srcSent)
      alignPW.println(alignment)
    }
    srcPW.close()
    alignPW.close()
  }
  
  def main(args: Array[String]): Unit = {
    argumentParser.parse(args, Config()) map { config =>
      
      basicTests(config)
      val storage = config.outputPrefix 
      
      val srcSents   = Preprocessing.prepareTrainingDataForUnknownWords(loadSents(config.sourceFN))
      val alignments = loadSents(config.alignmentFN)
      
      val trainingData = filtering(srcSents zip alignments)
      saveData(s"$storage/filteredSents", s"$storage/filteredAlignments", trainingData)
      
      val initGrammar = loadOrMakeInitGrammar(config.initIterationFN, trainingData, storage, config.threads)
      
      val stoppingCriteria = createStoppingCriterion(config)
      
      if(config.batchEM){
        // batch EM
        System.err.println("START Batch EM training")
        BatchEM.runTraining(
            stoppingCriteria,
            storage,
            trainingData,
            initGrammar,
            config.threads,
            config.threadBatchSize)
        System.err.println("DONE Batch EM training")
      }else{
        // online EM
        System.err.println("START Online EM training")
        OnlineEM.runTraining(
            stoppingCriteria,
            storage,
            trainingData,
            initGrammar,
            config.threads,
            config.threadBatchSize,
            config.onlineBatchSize)
        System.err.println("DONE Online EM training")
      }
      
      // do stuff
    } getOrElse {
      System.err.println("arguments are bad")
    }
  }

}


