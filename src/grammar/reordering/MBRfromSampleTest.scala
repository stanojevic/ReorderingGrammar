package grammar.reordering

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class MBRfromSampleTest extends FlatSpec with ShouldMatchers{

  "shit" should "not fail" in {
    val loc = "/home/milos/Downloads/exact_output_tiny_Bash_small"
    MBRfromSample.main(Array("--samplesFile", loc))
  }

}