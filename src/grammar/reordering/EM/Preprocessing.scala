package grammar.reordering.EM

import grammar.reordering.representation.Grammar
import beer.permutation.pet.representation.NonTerm
import beer.permutation.pet.representation.Term
import beer.permutation.pet.representation.TreeNode

object Preprocessing {
  
  private type Word = String
  
  def prepareTrainingDataForUnknownWords(
      sents:List[List[Word]],
      maxUnknownCount:Int=3) : List[List[Word]] = {
    var counts = scala.collection.mutable.Map[String, Int]().withDefaultValue(0)
    for(sent <- sents){
      for(word <- sent){
        counts(word) += 1
      }
    }
    
    val unknowns = counts.filter{_._2 <= maxUnknownCount}.keySet
    
    sents.map{ sent =>
      sent.map{ word =>
        if(unknowns contains word){
          Grammar.unknownToken
        }else{
          word
        }
      }
    }
  }
  
  def maxArity(a:Set[(Int, Int)]) : Int = {
    val n = a.map{_._1}.max
    val attachLeft = false // doesn't matter
    val attachLow  = false // doesn't matter
    var tree = AlignmentCanonicalParser.parse(n, a, attachLeft, attachLow)
    tree.map{
      case NonTerm(start:Int, end:Int, min:Int, max:Int, operator:List[Int], children:List[TreeNode]) =>
        operator.size
      case Term(position:Int, el:Int) =>
        0
    }.max
  }
  
  def numAlignedWords(a:Set[(Int, Int)]) : Int = {
    a.map{_._1}.toSet.size
  }

}
