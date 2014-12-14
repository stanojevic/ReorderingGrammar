package grammar.reordering

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import beer.permutation.pet.representation.NonTerm
import beer.permutation.pet.representation.Term
import grammar.reordering.alignment.AlignmentCanonicalParser

class TrainTest extends FlatSpec with ShouldMatchers{

  "running main" should "not fail" in {
    val arg = "-s data/top10_english -a data/top10_alignments -t 2 -b 10 --binarySplits 20 --narySplits 20 -i 50 -o batch_EM_grammars/"
    Train.main(arg.split(" +"))
  }

}