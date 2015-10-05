package grammar.reordering.alignment

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import beer.permutation.pet.representation.NonTerm
import beer.permutation.pet.representation.Term
import grammar.reordering.alignment.AlignmentCanonicalParser

class AlignmentParserTest extends FlatSpec with ShouldMatchers{

  "extract alignment" should "return correct alignment" in {
    val string = "1-1 2-0 4-1"
    val alignment = AlignmentCanonicalParser.extractAlignment(string)
    
    alignment should equal (List( (1,1) , (2,0) , (4,1) ) ) 
  }
  
  "alignment to avg positions" should "return correct avg positions" in {
    val a = Set( (1,1) , (2,0) , (2, 1) , (4,1) )
    val avg = AlignmentCanonicalParser.avgTargetPosition(6, a)
    val correct = Array(-1, 1, 0.5, -1, 1, -1)
    
    avg should equal (correct)
  }
  
  "avgPositions to minPermutation" should "return correct permutation" in {
    val avgPositions = List(-1.0, 1.0, 0.0, 0.0, -1.0, 1.0, -1.0, -1.0)
    val correctP = List(2, 0, 1, 3)
    val p = AlignmentCanonicalParser.findMinPermutation(avgPositions)
    p should equal (correctP)
  }
  
  "avg positions to permutation" should "return correct permutation" in {
    val avgPositions = Array(-1, 1, 0.5, -1, 1, -1)
    val p = AlignmentCanonicalParser.findMinPermutation(avgPositions.toList)
    val correctP = List(1, 0, 2)
    
    p should equal (correctP)
  }
  
  "alignment to tree" should "return correct tree" in {
    val a = Set( (1,2) , (2,1) , (3,0), (5,2), (6,3), (7,4) )
    val n = 10
    val correctPet = NonTerm(1, 3, 1, 3, List(1,2), List(
                       NonTerm(1, 2, 1, 2, List(2,1), List(
                           Term(1, 2),
                           Term(2, 1))),
                       Term(3, 3)))
                       

    var attachLeft = false
    var attachLow  = true
    val rightBranching = false
    var label = s"left=$attachLeft low=$attachLow rightBranching=$rightBranching"
    var tree = AlignmentCanonicalParser.parse(n, a, attachLeft, attachLow, rightBranching)
    AlignmentCanonicalParser.visualizeTree(tree, label)

    attachLeft = true
    attachLow  = true
    label = s"left=$attachLeft low=$attachLow rightBranching=$rightBranching"
    tree = AlignmentCanonicalParser.parse(n, a, attachLeft, attachLow, rightBranching)
    AlignmentCanonicalParser.visualizeTree(tree, label)

    attachLeft = false
    attachLow  = false
    label = s"left=$attachLeft low=$attachLow rightBranching=$rightBranching"
    tree = AlignmentCanonicalParser.parse(n, a, attachLeft, attachLow, rightBranching)
    AlignmentCanonicalParser.visualizeTree(tree, label)

    attachLeft = true
    attachLow  = false
    label = s"left=$attachLeft low=$attachLow rightBranching=$rightBranching"
    tree = AlignmentCanonicalParser.parse(n, a, attachLeft, attachLow, rightBranching)
    AlignmentCanonicalParser.visualizeTree(tree, label)

    println("done")
    System.in.read()
    //tree should equal (correctPet)
  }

}