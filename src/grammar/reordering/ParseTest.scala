package grammar.reordering

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class ParseTest extends FlatSpec with ShouldMatchers{

  "running parser" should "not fail" in {
    val arg = "--sentencesFile testingGrammars/corpus.en "+
              "--grammarFile testingGrammars/grammar_5.dephrased "+
              "--outPermutedStringFN testingGrammars/outPermutedString "+
              "--outTreeFN testingGrammars/outTree "+
              "--outQuasiPermFN testingGrammars/outQuasiPerm "+
              "--kBest 1 "+
              "--samplesNumber 20 "+
              "--lambdaPruning -1000 "+
              "--grammarPruning 0 "+
              "--threads 1 "+
              "--flushingSize 40"

    Parse.main(arg.split(" +"))
  }

}
