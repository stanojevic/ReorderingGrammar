package grammar.reordering.representation

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class ProbabilityTest extends FlatSpec with ShouldMatchers{

  "summation" should "be correct" in {
    val a = Probability(0.1)
    val b = Probability(0.1)
    val c = a+b
    println(c.toDouble)
  }

}