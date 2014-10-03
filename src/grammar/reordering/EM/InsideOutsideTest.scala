package grammar.reordering.EM

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import grammar.reordering.representation.IntMapping
import grammar.reordering.representation.Rule
import grammar.reordering.representation.NonTerm
import grammar.reordering.representation.Grammar
import grammar.reordering.representation.`package`.InnerRule
import grammar.reordering.representation.`package`.PretermRule

class InsideOutsideTest extends FlatSpec with ShouldMatchers{

  "initial iteration" should "not break" in {
    val alignment = "1-1 2-0 3-0 4-0"
    val sent = "Nekakva recenica koja nema mnogo smisla"
    
    val g = InsideOutside.initialIteration(List(sent), List(alignment))
    g.allRules.toList.sortBy(_.lhs).foreach{ rule =>
      // println(rule.toString(g.voc, g.nonTerms))
    }
  }

  "real iteration" should "not break" in {
    val alignment = "1-1 2-0 3-0 4-0"
    val sent = "Nekakva recenica koja nema mnogo smisla"

    val g = InsideOutside.initialIteration(List(sent), List(alignment))
    
    val batchSize = 4
    val parallel = false
    val g2 = InsideOutside.iteration(List(sent), List(alignment), g, batchSize, parallel)
    g2.allRules.toList.sortBy(_.lhs).foreach{ rule =>
      // println(rule.toString(g2.voc, g2.nonTerms))
    }
  }

  "real 5 iterations" should "not break" in {
    val alignment = "1-1 2-0 3-0 4-0"
    val sent = "Nekakva recenica koja nema mnogo smisla"

    val g = InsideOutside.initialIteration(List(sent), List(alignment))
    
    val batchSize = 4
    val parallel = false
    val g2 = InsideOutside.iteration(List(sent), List(alignment), g, batchSize, parallel)
    val g3 = InsideOutside.iteration(List(sent), List(alignment), g2, batchSize, parallel)
    val g4 = InsideOutside.iteration(List(sent), List(alignment), g3, batchSize, parallel)
    val g5 = InsideOutside.iteration(List(sent), List(alignment), g4, batchSize, parallel)
    val g6 = InsideOutside.iteration(List(sent), List(alignment), g5, batchSize, parallel)
    g6.allRules.toList.sortBy(_.lhs).foreach{ rule =>
      println(rule.toString(g6.voc, g6.nonTerms))
    }
  }

}