package grammar.reordering.alignment

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import scala.io.Source
import java.io.PrintWriter

class PhrasePairExtractorTest extends FlatSpec with ShouldMatchers{

  "minimal phrase extraction" should "not break" in {
    // val aStr = "1-3 2-4 3-6 4-6 5-6"
    // val sent = "Moje ime je Milos Stanojevic car"
    // val aStr = "0-0 1-0 0-1 3-2 2-3 3-3 3-4 4-5 8-6 8-7 8-8 8-9 9-9 10-10 7-11 7-12 6-13 6-14 19-17 15-18 18-18 19-18 20-18 14-19 16-19 13-20 14-21 21-24 21-25"
    // val sent = "his works were many , including not only chinese-style landscape paintings , but also portraits and pictures of flowers and birds ."
    val aStr = "0-0 1-0 1-1 2-2 3-3 4-4 5-4 6-4 6-5 6-6 7-6 8-6 10-6 9-7 9-8 9-9 10-10 10-11 10-12 11-13 12-19 12-20"
    val sent = "his bold compositions and strong brush strokes constituted an extremely distinctive style ."
    val a = AlignmentCanonicalParser.extractAlignment(aStr)
    val words = sent.split(" +").toList
    val n = words.size

    val phraseSpans = PhrasePairExtractor.findPhrases(a, n).sortBy(_._1._1)

    for(((e1, e2), (f1, f2)) <- phraseSpans){
      val phrase = words.drop(e1).take(e2-e1+1).mkString(" ")
      println(s"[$e1-$e2] [$f1-$f2] $phrase")
    }
    
    val (fakeSent, fakeAlign) = PhrasePairExtractor.fakeAlignmentAndFakeWords(words, phraseSpans)
    println(fakeSent.mkString(" "))
    println(fakeAlign.mkString(" "))
  }
}

object PhrasePairExtractorTest{
  
  def main(argv:Array[String]) : Unit = {
    if(argv.size != 5){
      System.err.println("usage: srcCorp alignFile outPhraseFile outSentFile outAlignFile")
      System.exit(-1)
    }
    val srcCorpus = argv(0)
    val alignment = argv(1)
    val outPhraseFile = argv(2)
    val outSentFile = argv(3)
    val outAlignFile = argv(4)
    
    val sentPW = new PrintWriter(outSentFile)
    val alignPW = new PrintWriter(outAlignFile)

    val srcLines = Source.fromFile(srcCorpus).getLines().toList
    val alignLines = Source.fromFile(alignment).getLines().toList
    val data = (srcLines zip alignLines)

    System.err.println("data loaded")
    
    val minPhrases = scala.collection.mutable.Map[String, Int]().withDefaultValue(0)
    
    var processed = 0
    val total = data.size
    for((src, al) <- data){
      val a = AlignmentCanonicalParser.extractAlignment(al)
      val words = src.split(" +").toList
      val n = words.size
      
      val phraseSpans = PhrasePairExtractor.findPhrases(a, n)
      
      for(((e1, e2), (f1, f2)) <- phraseSpans){
        val phrase = words.drop(e1).take(e2-e1+1).mkString(" ")
        minPhrases(phrase) = minPhrases(phrase) + 1
      }
      
      val (sent, aligns) = PhrasePairExtractor.fakeAlignmentAndFakeWords(words, phraseSpans)
      sentPW.println(sent.mkString(" "))
      alignPW.println(aligns.map{case (x,y) => s"$x-$y"}.mkString(" "))
      
      processed += 1
      if(processed%1000 == 0){
        val phrasesExtracted = minPhrases.size
        System.err.println(s"$processed/$total with $phrasesExtracted phrases extracted")
      }
    }
    
    System.err.println("START saving phrases")
    val pw = new PrintWriter(outPhraseFile)
    for((phrase, count) <- minPhrases){
      pw.println(s"$count\t$phrase")
    }
    pw.close()
    sentPW.close()
    alignPW.close()
    System.err.println("DONE saving phrases")
    
  }
}
