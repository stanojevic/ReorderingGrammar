package grammar.reordering.EM

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class GrammarSplitterTest extends FlatSpec with ShouldMatchers{
  "grammar splitter" should "not break" in {
    val alignment = "1-1 2-0 3-0 4-0"
    val sent = "Nekakva recenica koja nema mnogo smisla"
    val threads = 1
    val words = sent.split(" +").toList
    val tags = words.map{word =>
      val tag = "tag_"+word
      Map(tag -> 1.0)
    } // stupid trivial tag
    
    val g = InsideOutside.initialIteration(List((sent, alignment, tags)))
    val g1 = GrammarSplitter.split(g, threads)
    
    g1.save("grammar_testingGrammarSplitter.txt")

  }

}