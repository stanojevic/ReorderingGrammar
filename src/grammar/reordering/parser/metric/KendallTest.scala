package grammar.reordering.parser.metric

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import grammar.reordering.parser.SimpleTreeNode

class KendallTest extends FlatSpec with ShouldMatchers{

  "evaluation" should "be correct" in {
    val tree = SimpleTreeNode.fromPennString("(ROOT (P12 (P21 (P12 (tag_the the) (tag_engine engine) (tag_1 1) (tag_is is)) (P12 (P01 (tag_an an) (tag_internal internal)) (tag_combustion combustion) (tag_engine engine) (tag_which which) (tag_makes makes) (P21 (tag_use use) (tag_of of) (P12 (tag_thermal thermal) (tag_energy energy))) (tag_and and) (tag_drives drives) (tag_the the) (tag_two two) (tag_drive drive) (tag_wheels wheels) (tag_8 8) (tag_, ,) (tag_8 8) (P21 (tag_via via) (P12 (tag_the the) (tag_cvt cvt))) (tag_3 3) (tag_and and)) (P12 (tag_the the) (tag_drive drive) (tag_shaft shaft) (tag_7 7))) (tag_. .)))")
    System.err.println("tree prob : "+tree.subTreeP)
    System.err.println("length : "+tree.yieldOriginalSentence.size)
    System.err.println("sent : "+tree.yieldOriginalSentence)
    val metric = new Kendall()
    val slowScore = metric.score(tree, tree)
    System.err.println("slow "+slowScore)
    val featuresRef  = metric.extractFeatureCountsRef(tree)
    val featuresSys  = metric.extractFeatureCountsSys(tree)
    val fastScore = metric.scoreWithFeatures(featuresSys, featuresRef)
    System.err.println("fast "+fastScore)
  }

}