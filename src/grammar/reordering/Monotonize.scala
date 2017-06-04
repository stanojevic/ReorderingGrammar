package grammar.reordering

import scala.io.Source
import java.io.PrintWriter
import grammar.reordering.alignment.AlignmentCanonicalParser
import grammar.reordering.alignment.PhrasePairExtractor

object Monotonize {

  private case class Config(
      sourceFN : String = "",
      alignmentFN : String = "",
      attachLeft  : Boolean = true,
      simpleSprime : Boolean = false,
      
      outAlignmentsFN : String = null,
      outSentencesFN : String = null
  )

  private val argumentParser = new scopt.OptionParser[Config]("ReorderingGrammar") {

      head("ReorderingGrammar", "0.1")

      opt[String]('s', "sourceFile") required() action { (x, c) =>
        c.copy(sourceFN = x)
      }
      
      opt[String]('a', "alignmentsFile") required() action { (x, c) =>
        c.copy(alignmentFN = x)
      }
      
      opt[Boolean]("simpleSprime") action { (x, c) =>
        c.copy(simpleSprime = x)
      } text ("no -- use standard minimal phrase reorderin, yes -- use average target position")
      
      
      opt[Boolean]("nullAttachLeft") action { (x, c) =>
        c.copy(attachLeft = x)
      } required()
      
      opt[String]("outAlignments") action { (x, c) =>
        c.copy(outAlignmentsFN = x)
      }
      
      opt[String]("outSentences") action { (x, c) =>
        c.copy(outSentencesFN = x)
      } required()

      help("help") text("prints this usage text")

  }

  private def loadSents(file: String) : List[String] = Source.fromFile(file).getLines().toList
  
  def main(args: Array[String]) : Unit = {
    argumentParser.parse(args, Config()) map { config =>
      val rawSentences : List[String]  = loadSents(config.sourceFN)
      val rawAlignments : List[String] = loadSents(config.alignmentFN)


      val outSentPW = if (config.outSentencesFN == null) {
        null
      } else {
        new PrintWriter(config.outSentencesFN)
      }

      val outAlignPW = if (config.outAlignmentsFN == null) {
        null
      } else {
        new PrintWriter(config.outAlignmentsFN)
      }
      
      System.err.println("STARTED MONOTONIZING")
      
      var processed = 0
      val sentsCount = rawSentences.size
      
      (rawSentences zip rawAlignments).foreach{ case (sent, align) =>
        val a = AlignmentCanonicalParser.extractAlignment(align)
        val words = sent.split(" +").toList
        val n = words.size
        val (phrasedWords, phrasedAlignments) = if(config.simpleSprime){
          val newAlignment:Set[(Int, Int)] = simplifyAlignments(a)
          (words, newAlignment)
        }else{
          val spans = PhrasePairExtractor.findPhrases(a, n)
          PhrasePairExtractor.fakeAlignmentAndFakeWords(words, spans)
        }
        
        val (newSent, newA) = do_reordering(phrasedWords, phrasedAlignments, config.attachLeft)
        
        if(config.simpleSprime){
          outSentPW.println( newSent.mkString(" ") )
        }else{
          outSentPW.println(
              newSent.
                flatMap{PhrasePairExtractor.unfakePhrase(_)}.
                mkString(" ")
                )
        }
        if(outAlignPW != null){
          val dephrasedAlignment = convertPhrasalToNormalAlignment(newSent, newA)
          outAlignPW.println(dephrasedAlignment.map{case (i, j) => s"$i-$j"}.mkString(" "))
        }
        
        processed += 1
        if(processed % 1000 == 0){
          System.err.println(s"$processed/$sentsCount phrase merging")
        }
      }
      
      if (outSentPW  != null)  outSentPW.close()
      if (outAlignPW != null) outAlignPW.close()
      System.err.println("DONE MONOTONIZING")
      
      
    } getOrElse {
      System.err.println("arguments are bad")
    }
  }
  
  def convertPhrasalToNormalAlignment(phrasalSent:List[String], phrasalAlign:Set[(Int, Int)]) : Set[(Int, Int)] = {
    val phraseSizes = phrasalSent.map{PhrasePairExtractor.unfakePhrase(_).size}
    
    val sourceStartPositions = collection.mutable.Map[Int, Int]()
    var nextSrcStartPos = 0
    phrasalAlign.toList.sortBy{_._1}.foreach{ case (src, tgt) =>
      sourceStartPositions(src) = nextSrcStartPos
      nextSrcStartPos += phraseSizes(src)
    }
    
    val targetStartPositions = collection.mutable.Map[Int, Int]()
    var nextTgtStartPos = 0
    phrasalAlign.toList.sortBy{_._2}.foreach{ case (src, tgt) =>
      targetStartPositions(tgt) = nextTgtStartPos
      nextTgtStartPos += phraseSizes(src)
    }
    
    val newAlignments = collection.mutable.Set[(Int, Int)]()
    
    phrasalAlign.foreach{ case (src, tgt) =>
      for(i <- 0 until phraseSizes(src)){
        val link = (sourceStartPositions(src)+i, targetStartPositions(tgt)+i)
        newAlignments += link
      }
    }

    newAlignments.toSet
  }
  
  private def simplifyAlignments(oldA:Set[(Int, Int)]):Set[(Int, Int)] = {
    oldA.groupBy(_._1).map{ case (x, ys) =>
      val softTgtPos = ys.map{_._2}.sum.toDouble/ys.size
      (x, softTgtPos)
    }.toList.sortBy{_._2}.map{_._1}.zipWithIndex.toSet
  }
  
  def do_reordering(
      phrasedWords:List[String], 
      a:Set[(Int, Int)], 
      attachLeft:Boolean) : (List[String], Set[(Int, Int)]) = {
    val n = phrasedWords.size
    val mapping = Array.fill(n)(-1.0)
    
    a.groupBy(_._1).foreach{ case (i, points) =>
      val size = points.size.toDouble
      val target = points.map(_._2).sum/size
      mapping(i) = target
    }
    
    //find the leftmost aligned "word"
    var leftMostAligned = 0
    while(leftMostAligned < mapping.size && mapping(leftMostAligned)<0){
      leftMostAligned+=1
    }
    
    if(leftMostAligned < mapping.size && leftMostAligned>0){
      for(i <- 0 until leftMostAligned){
        mapping(i) = mapping(leftMostAligned)
      }
    }
    
    //find the rightmost aligned "word"
    var rightMostAligned = mapping.size-1
    while(rightMostAligned > 0 && mapping(rightMostAligned)<0){
      rightMostAligned-=1
    }
    
    if(rightMostAligned < mapping.size-1 && rightMostAligned>=0){
      for(i <- rightMostAligned+1 until mapping.size){
        mapping(i) = mapping(rightMostAligned)
      }
    }
    
    //solve the unaligned words in the middle
    if(attachLeft){
      for(i <- 1 until mapping.size){
        if(mapping(i)<0){
          mapping(i)=mapping(i-1)
        }
      }
    }else{
      for(i <- mapping.size-2 to 0 by -1){
        if(mapping(i)<0){
          mapping(i)=mapping(i+1)
        }
      }
    }
    
    
    // find the mapping
    val oldPosToNewPosMapping =
      (0 to mapping.size-1).sortBy(mapping(_)).zipWithIndex.toMap
      
    // new alignments
    val newA = a.map{case (i, j) => (oldPosToNewPosMapping(i), j)}
    
    // new words
    val newSent = phrasedWords.zipWithIndex.
                    map{ case (word, i) =>
                      (word, oldPosToNewPosMapping(i))
                    }.sortBy(_._2).map{_._1}
    
    (newSent, newA)
  }

}
