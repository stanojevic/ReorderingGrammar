package grammar.reordering.parser

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import grammar.reordering.parser.SimpleTreeNode
import grammar.reordering.parser.metric.Kendall

class MBRtest extends FlatSpec with ShouldMatchers{

  "evaluation" should "be correct" in {
    val tree1 = SimpleTreeNode.fromPennString("(ROOT (P12 (P21 (P12 (tag_the the) (tag_engine engine) (tag_1 1) (tag_is is)) (P12 (P01 (tag_an an) (tag_internal internal)) (tag_combustion combustion) (tag_engine engine) (tag_which which) (tag_makes makes) (P21 (tag_use use) (tag_of of) (P12 (tag_thermal thermal) (tag_energy energy))) (tag_and and) (tag_drives drives) (tag_the the) (tag_two two) (tag_drive drive) (tag_wheels wheels) (tag_8 8) (tag_, ,) (tag_8 8) (P21 (tag_via via) (P12 (tag_the the) (tag_cvt cvt))) (tag_3 3) (tag_and and)) (P12 (tag_the the) (tag_drive drive) (tag_shaft shaft) (tag_7 7))) (tag_. .)))")
    val tree2 = SimpleTreeNode.fromPennString("(ROOT (P12 (P21 (P12 (tag_the the) (tag_engine engine) (tag_1 1) (tag_is is)) (P12 (P01 (tag_an an) (tag_internal internal)) (tag_combustion combustion) (tag_engine engine) (tag_which which) (tag_makes makes) (P21 (tag_use use) (tag_of of) (P12 (tag_thermal thermal) (tag_energy energy))) (tag_and and) (tag_drives drives) (tag_the the) (tag_two two) (tag_drive drive) (tag_wheels wheels) (tag_8 8) (tag_, ,) (tag_8 8) (P21 (tag_via via) (P12 (tag_the the) (tag_cvt cvt))) (tag_3 3) (tag_and and)) (P21 (tag_the the) (tag_drive drive) (tag_shaft shaft) (tag_7 7))) (tag_. .)))")
    val tree3 = SimpleTreeNode.fromPennString("(ROOT (P12 (P21 (P12 (tag_the the) (tag_engine engine) (tag_1 1) (tag_is is)) (P12 (P01 (tag_an an) (tag_internal internal)) (tag_combustion combustion) (tag_engine engine) (tag_which which) (tag_makes makes) (P21 (tag_use use) (tag_of of) (P12 (tag_thermal thermal) (tag_energy energy))) (tag_and and) (tag_drives drives) (tag_the the) (tag_two two) (tag_drive drive) (tag_wheels wheels) (tag_8 8) (tag_, ,) (tag_8 8) (P21 (tag_via via) (P12 (tag_the the) (tag_cvt cvt))) (tag_3 3) (tag_and and)) (P21 (tag_the the) (tag_drive drive) (tag_shaft shaft) (tag_7 7))) (tag_. .)))")
    
    val listToRerank = List(tree1, tree2, tree3)
    val metricFunc = new Kendall()
    
    val res = MBR.rerankFast(listToRerank, metricFunc)
    for((tree, score) <- res){
      System.err.println(tree)
      System.err.println(score)
    }

  }

}
