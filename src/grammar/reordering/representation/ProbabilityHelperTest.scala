package grammar.reordering.representation

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class ProbabilityHelperTest extends FlatSpec with ShouldMatchers{

  "summing log probs" should "be correct" in {
    val p = 0.00000000023141614316
    val q = 0.000000000000513451346136
    val a = Math.log(p)
    val b = Math.log(q)
    
    val r1 = ProbabilityHelper.logSumExp(a, b)
    val r1e = Math.exp(r1)
    val r2 = Math.log(Math.exp(a)+Math.exp(b))
    val r2e = Math.exp(r2)
    val pq = p+q
    
    println("p+q = "+pq)
    println("exp(r1) = "+r1e+" error="+Math.abs(r1e-pq))
    println("exp(r2) = "+r2e+" error="+Math.abs(r2e-pq))

  }

  "summing with -Inf" should "be correct" in {
    //val p = 0.00000000023141614316
    //val p = 0
    val p = 1000*Double.MinPositiveValue 
    //val q = 0
    //val q = 0.000000000000513451346136
    val q = Double.MinPositiveValue 
    val a = Math.log(p)
    val b = Math.log(q)
    
    val r1 = ProbabilityHelper.logSumExp(a, b)
    val r1e = Math.exp(r1)
    val r2 = Math.log(Math.exp(a)+Math.exp(b))
    val r2e = Math.exp(r2)
    val pq = p+q
    
    println("p = "+p)
    println("q = "+q)
    println("p+q = "+pq)
    println("exp(r1) = "+r1e+" error="+Math.abs(r1e-pq))
    println("exp(r2) = "+r2e+" error="+Math.abs(r2e-pq))

  }

}
