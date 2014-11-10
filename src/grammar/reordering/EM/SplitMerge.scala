package grammar.reordering.EM

import grammar.reordering.representation.Grammar
import grammar.reordering.representation.IntMapping
import grammar.reordering.representation.NonTerm
import grammar.reordering.representation.`package`.Rule
import grammar.reordering.representation.InnerRule
import grammar.reordering.representation.PretermRule
import grammar.reordering.representation.Probability
import grammar.reordering.representation.Probability.sum
import scala.util.Random

object SplitMerge {

  private var latentLimit = Map[String, Int]()
  latentLimit += "ROOT" -> 1

   for(perm <- List("A", "N", "P01", "P10", "P12", "P21", "P2413", "P3142", "P24153", "P35142")){
     latentLimit += perm -> 1
   }

  for(motherSize <- List(2, 4, 5)){
    for(mothersChild <- 1 to motherSize){
      for(perm <- List("A", "N", "P01", "P10", "P12", "P21", "P2413", "P3142", "P24153", "P35142")){
        if(motherSize>2){
          // How many nonterms that cause combinatorial explosion?
          val nt = "M"+motherSize+"C"+mothersChild+perm
          latentLimit += nt -> 4
        }else{
          if(Set("A", "N") contains perm){
            // How many POS tags?
            val nt = "M"+motherSize+"C"+0+perm
            latentLimit += nt -> 1000
          }else{
            // How many nonterms that are binary?
            val nt = "M"+motherSize+"C"+0+perm
            latentLimit += nt -> 100
          }
        }
      }
    }
  }

  
  def split(oldG:Grammar, oldRuleExpectations:Map[Rule, Double]) : Grammar = {
    val oldVoc =  oldG.voc
    val oldNonTerms = oldG.nonTerms
    val oldLatentMappings = oldG.latentMappings
    
    var newLatentMappings = Map[NonTerm, List[NonTerm]]()
    val newNonTerms = new IntMapping()

    var newSplits = List[(String, String, String, Double, Double)]()
    
    val oldRules = oldG.allRules
    val oldExpectations:Map[String, Double] = oldRules.groupBy(rule => oldNonTerms(rule.lhs)).mapValues(rules => rules.toList.map{oldRuleExpectations(_)}.sum)
    
    for(origNToldInt <- oldLatentMappings.keys){
      val origNTstring = oldNonTerms(origNToldInt)
      val origNTnewInt = newNonTerms(origNTstring)

      val splitLimit = latentLimit(origNTstring)
      var currentLatentSize = oldLatentMappings(origNToldInt).size
      var currentLatentMappings = List[NonTerm]()

      for(origLToldInt <- oldLatentMappings(origNToldInt)){
        val origLTstring = oldNonTerms(origLToldInt)

        val latentNTnewInt = newNonTerms(origLTstring) // we have to add it because of possible merging later
        if(currentLatentSize < splitLimit && oldExpectations.getOrElse(origLTstring, 0.0) != 0.0){
          val latentNT1string = origLTstring+"_0"
          val latentNT1newInt = newNonTerms(latentNT1string)
          val latentNT2string = origLTstring+"_1"
          val latentNT2newInt = newNonTerms(latentNT2string)
          currentLatentMappings ::= latentNT1newInt
          currentLatentMappings ::= latentNT2newInt
          newSplits ::= (origLTstring, latentNT1string, latentNT2string, oldExpectations(origLTstring)/2, oldExpectations(origLTstring)/2)
          currentLatentSize += 1
        }else{
          currentLatentMappings ::= latentNTnewInt
        }
      }
      newLatentMappings += newNonTerms(origNTstring) -> currentLatentMappings
    }
    
    // CREATE NEW RULES !!! using Grammar.allCombinations
    val newRules:Set[Rule] = oldG.allRules.flatMap{
      case InnerRule(oldLhs, oldRhs, oldProb) =>
        val representations = (oldLhs::oldRhs).map{ oldNonTerm =>
          val nonTermString = oldNonTerms(oldNonTerm)
          if(newSplits.exists{_._1  == nonTermString}){
            val (mother, split1String, split2String, expect1, expect2) = newSplits.dropWhile{_._1  != nonTermString}.head
            List(newNonTerms(split1String), newNonTerms(split2String))
          }else{
            List(newNonTerms(nonTermString))
          }
        }
        
        val combinations = Grammar.allCombinations(representations)
        val combinationsSize = combinations.size //to speed it up a bit
        
        combinations.map{ case lhs::rhs =>
          var randomness = if(combinationsSize == 1) 0.0 else Random.nextDouble()/100 - 0.005
          while(oldProb.toDouble + randomness > 1.0 || oldProb.toDouble + randomness < 0.0){
            randomness = Random.nextDouble()/100 - 0.005
          }
          InnerRule(lhs, rhs, Probability(oldProb.toDouble + randomness))
        }
      case PretermRule(oldLhs, word, prob) =>
        val nonTermString = oldNonTerms(oldLhs)
        val representation = if(newSplits.exists{_._1  == nonTermString}){
            val (mother, split1String, split2String, expect1, expect2) = newSplits.dropWhile{_._1  != nonTermString}.head
            List(newNonTerms(split1String), newNonTerms(split2String))
          }else{
            List(newNonTerms(nonTermString))
          }
        representation.map{ case lhs =>
          var randomness = if(representation.size == 1) 0.0 else Random.nextDouble()/100 - 0.005
          while(prob.toDouble + randomness > 1.0 || prob.toDouble + randomness < 0.0){
            randomness = Random.nextDouble()/100 - 0.005
          }
          PretermRule(lhs, word, Probability(prob.toDouble+randomness))
        }
    }

    new Grammar(normalize(newRules), newLatentMappings, oldVoc, newNonTerms, newSplits)
  }
  
  def smoothSplits(oldG:Grammar) : Grammar = {
    val alpha = 0.01
    val newRules:Set[Rule] = oldG.allRules.groupBy{
      case rule:InnerRule   => (oldG.reverseLatentMappings(rule.lhs), rule.rhs)
      case rule:PretermRule => (oldG.reverseLatentMappings(rule.lhs), rule.word)
    }.toList.flatMap{
      case ((parentLhs:NonTerm, rhs:List[Any]), rules) =>
        val pBar = rules.asInstanceOf[Set[Rule]].map{_.prob.toDouble}.sum / rules.size
        rules.asInstanceOf[Set[InnerRule]].map{ case InnerRule(lhs, rhs, prob) =>
          InnerRule(lhs, rhs, Probability((1-alpha)*prob.toDouble + alpha*pBar))
        }.toList
      case ((parentLhs:NonTerm, word:NonTerm), rules) =>
        val pBar = rules.asInstanceOf[Set[Rule]].map{_.prob.toDouble}.sum / rules.size
        rules.asInstanceOf[Set[PretermRule]].map{ case PretermRule(lhs, word, prob) =>
          PretermRule(lhs, word, Probability((1-alpha)*prob.toDouble + alpha*pBar))
        }.toList
    }.toSet

    new Grammar(normalize(newRules), oldG.latentMappings, oldG.voc, oldG.nonTerms, oldG.latestSplits)
  }
  
  def merge(oldG:Grammar) : Grammar = {
//    oldG.latestSplits.map{
//      case (merged0:String, (split1:String, split2:String)) =>
//        // oldG.latentMappings()
//    }
    null
  }
  
  private def normalize(rules:Set[Rule]) : Set[Rule] =
    rules.groupBy(_.lhs).flatMap{ case (lhs, rules) =>
      val total = rules.toList.map{_.prob.toDouble}.sum
      rules.map{
        case InnerRule(_, rhs, prob) => InnerRule(lhs, rhs, Probability(prob.toDouble/total))
        case PretermRule(_, word, prob) => PretermRule(lhs, word, Probability(prob.toDouble/total))
      }
    }.toSet

}
