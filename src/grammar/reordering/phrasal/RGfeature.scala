package grammar.reordering.phrasal

import edu.stanford.nlp.mt.decoder.feat.DerivationFeaturizer
import edu.stanford.nlp.mt.decoder.feat.FeaturizerState
import edu.stanford.nlp.mt.util.IString
import edu.stanford.nlp.mt.util.FeatureValue
import edu.stanford.nlp.mt.util.Featurizable
import edu.stanford.nlp.mt.util.Sequence
import edu.stanford.nlp.mt.tm.ConcreteRule
import scala.io.Source
import grammar.reordering.parser.SimpleTreeNode

class RGfeature (
    fileWithRisks:String,
    fileWithViterbiPermutations:String,
    leftMostLink:Boolean,
    unalignedAttachLeft:Boolean ) extends DerivationFeaturizer[IString, String] {
  
  val featureName = "rg_score"
  
  type Matrix = Array[Array[Double]]
  private val allRisks : Array[Matrix] =
    loadRisksFromFile(fileWithRisks, fileWithViterbiPermutations)
  
  private def loadRisksFromFile(
    fileWithRisks:String,
    fileWithViterbiPermutations:String) : Array[Matrix] = {
    
    var allRisks = List[Matrix]()
    
    val riskLines = Source.fromFile(fileWithRisks).getLines()

    val permLines = if(fileWithViterbiPermutations != null && fileWithViterbiPermutations != ""){
      Source.fromFile(fileWithViterbiPermutations).getLines()
    }else{
      null
    }
    
    for(riskLine <- riskLines){
      
      val riskies = riskLine.split(" +").toList.map{ skipField =>
        val fields = skipField.split(":")
        val firstInd  = fields(0).toInt
        val secondInd = fields(1).toInt
        val risk      = fields(2).toDouble
        (firstInd, secondInd, risk)
      }
      val n = riskies.map{_._1}.max + 1
      val riskMatrix = Array.ofDim[Double](n, n)
      
      val indexMapping = new Array[Int](n)
      if(permLines != null){
        val permLine = permLines.next()
        val permutation = permLine.substring(permLine.indexOf(" ||| ")+5).
          split(" +").toList.
          map(_.toInt). // conver to the right type
          map{x => if(x<0) x+SimpleTreeNode.UNALIGNED_SHIFT else x} // handle unaligned words
        for((value, position) <- permutation.zipWithIndex){
          indexMapping(value) = position
        }
      }

      for((firstInd, secondInd, risk) <- riskies){
        if(permLines == null){
          riskMatrix(firstInd)(secondInd) = risk
        }else{
          riskMatrix(indexMapping(firstInd))(indexMapping(secondInd)) = risk
        }
      }
      
      allRisks ::= riskMatrix
    }
    
    allRisks.reverse.toArray
  }
  
  override
  def initialize(
      sourceInputId:Int,
      ruleList : java.util.List[ConcreteRule[IString, String]],
      source:Sequence[IString]
      ) : Unit = {
    
  }
  
  override
  def featurize(f:Featurizable[IString, String]) : java.util.List[FeatureValue[String]] = {
    val alignment = f.rule.abstractRule.alignment
    
    val src_start = f.sourcePosition
    val src_phrase_size = f.sourcePhrase.size()
    val fuzzyAlignment = Array.fill(src_phrase_size)(-1.0)
    
    for(tgt_pos <- 0 until f.targetPhrase.size()){
      var src_positions = alignment.t2s(tgt_pos)
        
      if(src_positions != null){
        for(src_pos <- src_positions){
          if(  fuzzyAlignment(src_pos) == -1     ||
               ( leftMostLink && tgt_pos < fuzzyAlignment(src_pos)) ||
               (!leftMostLink && tgt_pos > fuzzyAlignment(src_pos))
               ){
            fuzzyAlignment(src_pos) = tgt_pos
          }
        }
      }
    }
    
    if(unalignedAttachLeft){
      if(fuzzyAlignment(fuzzyAlignment.size-1) == -1){
        fuzzyAlignment(fuzzyAlignment.size-1) = Double.MaxValue
      }
      for(src_pos <- fuzzyAlignment.size-2 to 0 by -1){
        if(fuzzyAlignment(src_pos) == -1){
          fuzzyAlignment(src_pos) = fuzzyAlignment(src_pos+1)
        }
      }
    }else{
      for(src_pos <- 0 until fuzzyAlignment.size){
        if(fuzzyAlignment(src_pos) == -1){
          fuzzyAlignment(src_pos) = fuzzyAlignment(src_pos-1)
        }
      }
    }
    
    val sortedSrcPositions = (0 until fuzzyAlignment.size).
                                      sortBy{fuzzyAlignment(_)}.
                                      map{_+src_start}.
                                      toList
    
    val riskMatrix = allRisks(f.sourceInputId)
    val oldState:RGstate = f.prior match {
      case null => RGstate.initState(f.sourceSentence.size())
      case _    => f.prior.getState(this).asInstanceOf[RGstate]
    }

    var rgScore = 0.0

    var oldUnprocessed = oldState.notProcessed
    var newUnprocessed = List[Int]()
    for(src_pos <- sortedSrcPositions){
      for(src_after_pos <- oldUnprocessed){
        if(src_after_pos != src_pos){
          newUnprocessed ::= src_after_pos
          rgScore += riskMatrix(src_pos)(src_after_pos)
        }
      }
      oldUnprocessed = newUnprocessed
      newUnprocessed = List[Int]()
    }
    
    f.setState(this, new RGstate(newUnprocessed))
    
    val features = new java.util.LinkedList[FeatureValue[String]]
    features.add(new FeatureValue[String](featureName, rgScore));
    
    features
  }
  
  class RGstate (val notProcessed:List[Int]) extends FeaturizerState {

    // TODO
    // optimize this thing that runs in O(n)
    override
    def equals(o:Any) : Boolean = {
      notProcessed == o
    }
    
    lazy private val myHashCode = notProcessed.hashCode()
    
    // TODO
    // optimize this thing that runs in O(n)
    override
    def hashCode() : Int = {
      myHashCode
    }
    
  }
  
  object RGstate {
    
    def initState(srcSize:Int) : RGstate = {
      new RGstate((0 until srcSize).toList)
    }
    
  }

}
