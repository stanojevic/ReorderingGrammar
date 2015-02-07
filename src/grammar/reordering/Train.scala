package grammar.reordering

import grammar.reordering.representation.Grammar
import scala.io.Source
import grammar.reordering.EM.InsideOutside
import grammar.reordering.EM.GrammarSplitter
import grammar.reordering.representation.Probability
import grammar.reordering.EM.BatchEM
import grammar.reordering.EM.OnlineEM
import java.io.File
import grammar.reordering.alignment.Preprocessing
import grammar.reordering.alignment.AlignmentCanonicalParser
import java.io.PrintWriter
import grammar.reordering.representation.POSseq
import grammar.reordering.alignment.PhrasePairExtractor

object Train {
  
  private case class Config(
      sourceFN : String = "",
      alignmentFN : String = "",
      wordClassFile : String = null,
      outputPrefix : String = "",
      threads: Int = 1,
      batchEM: Boolean = true,
      binarySplits: Int = 10,
      narySplits  : Int = 1,
      onlineBatchSize : Int = 10000,
      threadBatchSize : Int = 1000,
      hardEMbestK : Int = -1,
      randomnessInEstimation : Double = 0.0,
      onlineAlpha:Double = 0.6,
      initGrammarFN : String = null,
      iterations : Int = 30,
      convergenceThreshold : Double = -1,
      attachLeft  : Boolean = true,
      attachRight : Boolean = true,
      attachTop   : Boolean = true,
      attachBottom: Boolean = true
  )
  
  private val argumentParser = new scopt.OptionParser[Config]("ReorderingGrammar") {

      head("ReorderingGrammar", "0.1")

      opt[String]('s', "sourceFile") required() action { (x, c) =>
        c.copy(sourceFN = x)
      }
      
      opt[String]('a', "alignmentsFile") required() action { (x, c) =>
        c.copy(alignmentFN = x)
      }
      
      opt[String]('c', "wordClassFile") action { (x, c) =>
        c.copy(wordClassFile = x)
      }
      
      opt[String]('o', "outputPrefix") required() action { (x, c) =>
        c.copy(outputPrefix = x)
      }
      
      opt[String]("binarySplits") required() action { (x, c) =>
        c.copy(binarySplits = x.toInt)
      }
      
      opt[String]("narySplits") required() action { (x, c) =>
        c.copy(narySplits = x.toInt)
      }
      
      opt[Int]('t', "threads") action { (x, c) =>
        c.copy(threads = x)
      }
      
      opt[String]('g', "initGrammarFN") action { (x, c) =>
        c.copy(initGrammarFN = x)
      }
      
      opt[Boolean]("nullAttachLeft") action { (x, c) =>
        c.copy(attachLeft = x)
      }
      
      opt[Boolean]("nullAttachRight") action { (x, c) =>
        c.copy(attachRight = x)
      }
      
      opt[Boolean]("nullAttachTop") action { (x, c) =>
        c.copy(attachTop = x)
      }
      
      opt[Boolean]("nullAttachBottom") action { (x, c) =>
        c.copy(attachBottom = x)
      }
      
      opt[Int]("hard_EM_best_K") action { (x, c) =>
        c.copy(hardEMbestK = x)
      } text ("How many Kbest for hard EM iterations (first iteration is always soft EM; for <=0 only soft EM will be used always)")
      
      opt[Double]("randomnessInEstimation") action { (x, c) =>
        c.copy(randomnessInEstimation = x)
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

  
  
  private def makeInitGrammar(
      trainingData:List[(String, String, POSseq)],
      grammarOutputPrefix:String,
      binarySplits:Int,
      narySplits:Int,
      threads:Int,
      attachLeft:Boolean,
      attachRight:Boolean,
      attachTop:Boolean,
      attachBottom:Boolean) : Grammar = {
    System.err.println("START creating init grammar")
    val initG = InsideOutside.initialIteration(trainingData, attachLeft, attachRight, attachTop, attachBottom)
    initG.save(grammarOutputPrefix+"/nonSplittedGrammar", dephrased=false)
    System.err.println("DONE creating init grammar")
    System.err.println("START splitting init grammar")
    System.err.println(s"splitting binary into $binarySplits")
    System.err.println(s"splitting nary into $narySplits")
    val splittingConfig = GrammarSplitter.computeDefaultCategorySplitsConfiguration(binarySplits, narySplits)
    val splittedGrammar = GrammarSplitter.split(initG, threads, splittingConfig)
    System.err.println("DONE splitting init grammar")
    splittedGrammar.save(grammarOutputPrefix+"/initGrammar", dephrased=false)

    System.err.println("init grammar is saved")
    splittedGrammar
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
  
  private def filtering(oldData : List[(String, String, POSseq)]) : List[(String, String, POSseq)] = {
    System.err.println("STARTED filtering")
    val maxAllowedArity = 5
    var processed = 0
    val newData = oldData.filter{ case (_, alignment, _) =>
      processed += 1
      if(processed % 10000 == 0){
        System.err.println(processed)
      }
      val a = AlignmentCanonicalParser.extractAlignment(alignment)
      val arity = Preprocessing.maxArity(a)

      arity <= maxAllowedArity && Preprocessing.numAlignedWords(a) >=2
    }
    System.err.println("DONE filtering")
    System.err.println("kept "+newData.size+" out of "+oldData.size)
    
    newData
  }
  
  private def saveData(srcFN:String, alignFN:String, posFN:String, data:List[(String, String, POSseq)]) : Unit = {
    val srcPW = new PrintWriter(srcFN)
    val alignPW = new PrintWriter(alignFN)
    val posPW = new PrintWriter(posFN)
    for((srcSent, alignment, pos) <- data){
      srcPW.println(srcSent)
      alignPW.println(alignment)
      posPW.println(pos.map{_.keys.head})
    }
    posPW.close()
    alignPW.close()
    srcPW.close()
  }
  
  private def wordAsItselfSequence(srcSents:List[String]) : List[POSseq] = {
    srcSents.map{ sent =>
      sent.split(" +").map{ word =>
        val tag = "tag_"+word.replaceAllLiterally("*", "STAR")
        Map( tag -> 1.0 )
      }.toList
    }
  }

  private def wordClassSequence(srcSents:List[String], alignments:List[String], wordClassFile:String) : List[POSseq] = {
    val mapping = scala.collection.mutable.Map[String, Int]()
    Source.fromFile(wordClassFile).getLines().foreach{ line =>
      val res = line.split("\t")
      mapping += res(0) -> res(1).toInt
    }
    var posTagged = List[List[scala.collection.Map[String,Double]]]()
    for((sent, align) <- (srcSents zip alignments)){
      val words = sent.split(" +").toList
      val aLinks = AlignmentCanonicalParser.extractAlignment(align)
      val a = AlignmentCanonicalParser.avgTargetPosition(words.size, aLinks)
      val posSeq = words.zipWithIndex.map{ case (word, i) =>
        val initStr = if(a(i) < 0){
          "tag_N_"
        }else{
          "tag_A_"
        }
        val pos = if(mapping contains word){
          initStr+mapping(word)
        }else{
          initStr+"no_cluster"
        }
        Map(pos -> 1.0)
      }
      posTagged ::= posSeq
    }
    posTagged.reverse
  }
  
  private def firstIterationNumber(initGrammarFN:String) : Int = {
    if(initGrammarFN == null || initGrammarFN == ""){
      0
    }else{
      initGrammarFN.split("_").last.toInt+1
    }
  }
  
  private def mergePhrases(sents:List[String], aligns:List[String]) : (List[String], List[String]) = {
    var newSents = List[String]()
    var newAligns = List[String]()

    val sentsCount = sents.size
    var processed = 0
    (sents zip aligns).foreach{ case (sent, align) =>
      val a = AlignmentCanonicalParser.extractAlignment(align)
      val words = sent.split(" +").toList
      val n = words.size
      val spans = PhrasePairExtractor.findPhrases(a, n)
      val (newWords , newA) = PhrasePairExtractor.fakeAlignmentAndFakeWords(words, spans)
      newSents ::= newWords.mkString(" ")
      newAligns ::= newA.map{case (i, j) => s"$i-$j"}.mkString(" ")
      processed += 1
      if(processed % 1000 == 0){
        System.err.println(s"$processed/$sentsCount phrase merging")
      }
    }
    (newSents.reverse, newAligns.reverse)
  }
  
  def main(args: Array[String]): Unit = {
    argumentParser.parse(args, Config()) map { config =>
      
      basicTests(config)
      val storage = config.outputPrefix 
      val rawSentences : List[String] = loadSents(config.sourceFN)
      val rawAlignments : List[String] = loadSents(config.alignmentFN)
      
      val (phraseMergedSentences, phraseMergedAlignments) = mergePhrases(rawSentences, rawAlignments)

      var trainingData : List[(String, String, POSseq)] = null
      var initGrammar  : Grammar = null
      var firstIterNumber = 0
      
      if(config.initGrammarFN == null){
        // we are not continuing previous training but starting from scrach
        val srcSents     : List[String] = Preprocessing.prepareTrainingDataForUnknownWords(phraseMergedSentences)
        
        val posSequences : List[POSseq] = if(config.wordClassFile != null) {
          wordClassSequence(srcSents, phraseMergedAlignments, config.wordClassFile)
        }else{
          wordAsItselfSequence(srcSents)
        }

        trainingData = filtering(Preprocessing.zip3(srcSents, phraseMergedAlignments, posSequences))
        firstIterNumber = 0

        initGrammar = makeInitGrammar(trainingData, storage, config.binarySplits, config.narySplits, config.threads, config.attachLeft, config.attachRight, config.attachTop, config.attachBottom)
      }else{
        initGrammar = Grammar.loadFromFile(config.initGrammarFN)
        firstIterNumber = firstIterationNumber(config.initGrammarFN)
        
        System.err.println("filtering by grammar; RETHINK this if you are training on different data now")
        val srcSents     : List[String] = Preprocessing.prepareDataForUnknownWordsGivenGrammar(phraseMergedSentences, initGrammar)

        val posSequences : List[POSseq] = if(config.wordClassFile != null) {
          wordClassSequence(srcSents, phraseMergedAlignments, config.wordClassFile)
        }else{
          wordAsItselfSequence(srcSents)
        }
        
        trainingData = filtering(Preprocessing.zip3(srcSents, phraseMergedAlignments, posSequences))
      }
      
      saveData(s"$storage/filteredSents", s"$storage/filteredAlignments", s"$storage/filteredPos", trainingData)
      
      System.err.println(s"FIRST ITER NUMBER IS $firstIterNumber")
      
      val stoppingCriteria = createStoppingCriterion(config)
      
      if(config.batchEM){
        // batch EM
        System.err.println("START Batch EM training")
        BatchEM.runTraining(
            stoppingCriteria,
            storage,
            trainingData,
            initGrammar,
            firstIterNumber,
            config.threads,
            config.threadBatchSize,
            config.randomnessInEstimation,
            config.hardEMbestK,
            config.attachLeft,
            config.attachRight,
            config.attachTop,
            config.attachBottom
            )
        System.err.println("DONE Batch EM training")
      }else{
        // online EM
        System.err.println("START Online EM training")
        OnlineEM.runTraining(
            stoppingCriteria,
            storage,
            trainingData,
            initGrammar,
            firstIterNumber,
            config.threads,
            config.threadBatchSize,
            config.onlineBatchSize,
            config.randomnessInEstimation,
            config.onlineAlpha,
            config.attachLeft,
            config.attachRight,
            config.attachTop,
            config.attachBottom
            )
        System.err.println("DONE Online EM training")
      }
      
      // do stuff
    } getOrElse {
      System.err.println("arguments are bad")
      System.exit(-1)
    }
  }

}


