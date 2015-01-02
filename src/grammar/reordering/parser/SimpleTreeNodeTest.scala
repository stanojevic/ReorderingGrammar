package grammar.reordering.parser

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class SimpleTreeNodeTest extends FlatSpec with ShouldMatchers{

  "dotting the tree" should "not fail" in {
    // val s = "(S (N Milos) (VP (V je) (AdjP budala)))"
    val s = "(ROOT (P21 (tag_milos Milos) (P01_32 (P01*1_32 (tag_je je)) (P01*2_32 (tag_budala budala)))))"
    val sent = "Milos je budala".split(" ").toList
    val graphLabel = "testing"
    val node = SimpleTreeNode.fromPennString(s)
    node.visualize(sent, graphLabel)
    // System.in.read()
  }
  
  "permuting the tree" should "not fail" in {
    val s = "(ROOT (P2413_324 (P21 (P21*242 (tag_Milos Milos)) (P21*32 (tag_je je))) (tag_car car) (tag_i i) (tag_. .)))"
    val node = SimpleTreeNode.fromPennString(s)
    println(node.yieldPermutationWithUnaligned)
    println(node.yieldReorderedWithoutUnaligned())
    println(node.yieldReorderedWithUnaligned())
  }

}