package grammar.reordering.EM

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class GrammarSplitterTest extends FlatSpec with ShouldMatchers{
  "grammar splitter" should "not break" in {
    val alignment = "1-1 2-0 3-0 4-0"
    val sent = "Nekakva recenica koja nema mnogo smisla"
    val threads = 1
    
    val g = InsideOutside.initialIteration(List(sent) zip List(alignment))
    val g1 = GrammarSplitter.split(g, threads)
    
    g1.save("grammar_testingGrammarSplitter.txt")

  }

}