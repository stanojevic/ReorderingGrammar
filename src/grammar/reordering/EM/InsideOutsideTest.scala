package grammar.reordering.EM

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import grammar.reordering.representation.IntMapping
import grammar.reordering.representation.Rule
import grammar.reordering.representation.NonTerm
import grammar.reordering.representation.Grammar
import grammar.reordering.representation.`package`.InnerRule
import grammar.reordering.representation.`package`.PretermRule
import grammar.reordering.representation.Probability

class InsideOutsideTest extends FlatSpec with ShouldMatchers{

  "initial iteration" should "not break" in {
    val alignment = "1-1 2-0 3-0 4-0"
    val sent = "Nekakva recenica koja nema mnogo smisla"
    
    val g = InsideOutside.initialIteration(List(sent) zip List(alignment))
    // println(g)
  }

}
