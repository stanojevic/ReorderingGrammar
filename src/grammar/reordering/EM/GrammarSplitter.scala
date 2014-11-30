package grammar.reordering.EM

import grammar.reordering.representation.Grammar
import grammar.reordering.representation.Rule
import grammar.reordering.representation.InnerRule
import grammar.reordering.representation.PretermRule
import grammar.reordering.representation.Probability
import grammar.reordering.representation.NonTerm
import grammar.reordering.representation.IntMapping
import scala.util.Random

object GrammarSplitter {
  
  val defaultCategorySplitsConfiguration : Map[String, Int] = computeDefaultCategorySplitsConfiguration()
  
  def computeDefaultCategorySplitsConfiguration() : Map[String, Int] = {
    var latentLimit = Map[String, Int]()
    latentLimit += "ROOT" -> 1
  
    // // i'm not sure if this is needed
    // for(perm <- List("A", "N", "P01", "P10", "P12", "P21", "P2413", "P3142", "P24153", "P25314", "P42513", "P41352", "P35142", "P31524")){
    //   latentLimit += perm -> 1
    // }
  
    for(motherSize <- List(2, 4, 5)){
      for(mothersChild <- 1 to motherSize){
        for(perm <- List("A", "N", "P01", "P10", "P12", "P21", "P2413", "P3142", "P24153", "P25314", "P42513", "P41352", "P35142", "P31524")){
          if(motherSize>2){
            // How many nonterms that cause combinatorial explosion?
            val nt = "M"+motherSize+"C"+mothersChild+perm
            latentLimit += nt -> 4
          }else{
            if(perm == "A" || perm == "N"){
              // How many POS tags?
              val nt = "M"+motherSize+"C"+0+perm
              latentLimit += nt -> 20
            }else{
              // How many nonterms that are binary?
              val nt = "M"+motherSize+"C"+0+perm
              latentLimit += nt -> 30
            }
          }
        }
      }
    }
    
    latentLimit
  }


  def split(
      oldG:Grammar,
      categorySplits:Map[String, Int]=defaultCategorySplitsConfiguration
      ) : Grammar = {
    
    val oldVoc = oldG.voc
    val oldNonTerms = oldG.nonTerms
    val oldLatentMappings = oldG.latentMappings
    
    val newNonTerms = new IntMapping()

    var newLatentMappings = Map[NonTerm, List[NonTerm]]()
    for((motherNonTermString, splits) <- categorySplits){
      if(splits == 1){
        newLatentMappings += newNonTerms(motherNonTermString) -> List(newNonTerms(motherNonTermString))
      }else{
        newLatentMappings += newNonTerms(motherNonTermString) -> (0 until splits).map{ i => newNonTerms(motherNonTermString+"_"+i) }.toList
      }
    }
    
    var processed = 0
    val oldGsize = oldG.allRules.size
    val newRules:Set[Rule] = oldG.allRules.flatMap{
      case rule @ InnerRule(oldLhs, oldRhs, oldProb) =>
        processed += 1
        if(processed % 100 == 0){
          System.err.println(processed+"/"+oldGsize)
        }
        val representations = (oldLhs::oldRhs).map{ oldNonTerm =>
          val nonTermString = oldNonTerms(oldNonTerm)

          val numOfSplits = categorySplits(nonTermString)
          if(numOfSplits == 1){
            List(newNonTerms(nonTermString))
          }else{
            (0 until numOfSplits).map{ i => newNonTerms(nonTermString+"_"+i) }.toList
          }
        }
        
        val combinations = Grammar.allCombinations(representations)
        val combinationsSize = combinations.size // to speed it up a bit
        
        combinations.map{ case lhs::rhs =>
          var randomness = if(combinationsSize == 1) 0.0 else Random.nextDouble()/100 - 0.005
          while(oldProb.toDouble + randomness > 1.0 || oldProb.toDouble + randomness < 0.0){
            randomness = Random.nextDouble()/100 - 0.005
          }
          InnerRule(lhs, rhs, Probability(oldProb.toDouble + randomness))
        }
      case rule @ PretermRule(oldLhs, word, prob) =>
        processed += 1
        if(processed % 100 == 0){
          System.err.println(processed+"/"+oldGsize)
        }
        val nonTermString = oldNonTerms(oldLhs)
        val numOfSplits = categorySplits(nonTermString)
        val representation = if(numOfSplits == 1){
                               List(newNonTerms(nonTermString))
                             }else{
                               (0 until numOfSplits).map{ i => newNonTerms(nonTermString+"_"+i) }.toList
                             }
        // System.err.println("num of new latent rules: " + representation.size)
        representation.map{ case lhs =>
          var randomness = if(representation.size == 1) 0.0 else Random.nextDouble()/100 - 0.005
          while(prob.toDouble + randomness > 1.0 || prob.toDouble + randomness < 0.0) {
            randomness = Random.nextDouble()/100 - 0.005
          }
          PretermRule(lhs, word, Probability(prob.toDouble+randomness))
        }
    }.toSet
    
    val newG = new Grammar(normalize(newRules), newLatentMappings, oldVoc, newNonTerms)
    newG
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

    new Grammar(normalize(newRules), oldG.latentMappings, oldG.voc, oldG.nonTerms)
  }

  private def normalize(rules:Set[Rule]) : Set[Rule] = {
    System.err.println("START normalizing the rules and createing Grammar object")
    var processed = 0
    val lhsNum = rules.map{_.lhs}.toSet.size
    val normalizedRules :Set[Rule] = rules.groupBy(_.lhs).flatMap{ case (lhs, rules) =>
      processed += 1
      if(processed % 10 == 0){
        System.err.println(processed + "/" + lhsNum + " lhs")
      }
      val total = rules.toList.map{_.prob.toDouble}.sum
      rules.map{
        case InnerRule(_, rhs, prob) => InnerRule(lhs, rhs, Probability(prob.toDouble/total))
        case PretermRule(_, word, prob) => PretermRule(lhs, word, Probability(prob.toDouble/total))
      }
    }.toSet
    System.err.println("DONE normalizing the rules and createing Grammar object")
    normalizedRules
  }

}
