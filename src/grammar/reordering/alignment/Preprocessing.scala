package grammar.reordering.alignment

import grammar.reordering.representation.Grammar
import grammar.reordering.representation.POSseq
import beer.permutation.pet.representation.NonTerm
import beer.permutation.pet.representation.Term
import beer.permutation.pet.representation.TreeNode

object Preprocessing {
  
  def zip3(x:List[String], y:List[String], z:List[POSseq]) : List[(String, String, POSseq)] = {
    var res = List[(String, String, POSseq)]()
    var remainingX = x
    var remainingY = y
    var remainingZ = z
    while( ! remainingX.isEmpty){
      res ::= (remainingX.head, remainingY.head, remainingZ.head)
      remainingX = remainingX.tail
      remainingY = remainingY.tail
      remainingZ = remainingZ.tail
    }
    
    res.reverse
  }
  
  private type Word = String
  
  def prepareDataForUnknownWordsGivenGrammar(sents:List[String] , g:Grammar) : List[String] = {
    sents.map{sent =>
      sent.split(" +").toList.map{ word =>
        if(g.voc.contains(word)){
          word
        }else{
          Grammar.unknownToken
        }
      }.mkString(" ")
    }
  }

  def prepareTrainingDataForUnknownWords( sents : List[String], maxUnknownCount:Int = 3) : List[String] = {
    prepareTrainingDataForUnknownWordsDetailed(sents.map{_.split(" +").toList}, maxUnknownCount).map{_.mkString(" ")}
  }
  
  private def prepareTrainingDataForUnknownWordsDetailed(
      sents:List[List[Word]],
      maxUnknownCount:Int) : List[List[Word]] = {
    var counts = scala.collection.mutable.Map[String, Int]().withDefaultValue(0)
    for(sent <- sents){
      for(word <- sent){
        counts(word) += 1
      }
    }
    
    val unknowns = counts.filter{_._2 <= maxUnknownCount}.keySet
    
    sents.map{ sent =>
      sent.map{ word =>
        if(word.contains("[[[") && word.contains("]]]")){
          // it is actually a merged phrase
          word
        }else if(unknowns contains word){
          Grammar.unknownToken
        }else{
          word
        }
      }
    }
  }
  
  def maxArity(a:Set[(Int, Int)]) : Int = {
    val n = a.map{_._1}.max + 1
    val attachLeft = false // doesn't matter
    val attachLow  = false // doesn't matter
    def maxArityRec(node:TreeNode) : Int = node match {
      case NonTerm(start:Int, end:Int, min:Int, max:Int, operator:List[Int], children:List[TreeNode]) =>
        (children.size :: children.map{maxArityRec(_)}).max
      case Term(position:Int, el:Int) =>
        0
    }
    var tree = AlignmentCanonicalParser.parse(n, a, attachLeft, attachLow)
    val arity = maxArityRec(tree)
    arity
  }
  
  def numAlignedWords(a:Set[(Int, Int)]) : Int = {
    a.map{_._1}.toSet.size
  }

}
