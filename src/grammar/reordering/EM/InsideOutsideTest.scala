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
    val words = sent.split(" +").toList
    val tags = words.map{word =>
      val tag = "tag_"+word
      Map(tag -> 1.0)
    } // stupid trivial tag
    
    val attachLeft = true
    val attachRight = true
    val attachTop = true
    val attachBottom = true
    
    val g = InsideOutside.initialIteration(List((sent, alignment, tags)), attachLeft, attachRight, attachTop, attachBottom)
    // println(g)
  }

}
