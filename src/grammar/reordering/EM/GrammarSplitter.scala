package grammar.reordering.EM

import grammar.reordering.representation.Grammar
import grammar.reordering.representation.Rule
import grammar.reordering.representation.InnerRule
import grammar.reordering.representation.PretermRule
import grammar.reordering.representation.Probability
import grammar.reordering.representation.NonTerm
import grammar.reordering.representation.IntMapping
import scala.util.Random
import scala.collection.parallel.ForkJoinTaskSupport

object GrammarSplitter {
  
  val defaultCategorySplitsConfigurationUnmarked : Map[String, Int] = computeDefaultCategorySplitsConfiguration(false)
  val defaultCategorySplitsConfigurationMarked : Map[String, Int] = computeDefaultCategorySplitsConfiguration(true)
  
  def computeDefaultCategorySplitsConfiguration(markChildOrder:Boolean) : Map[String, Int] = {
    var latentLimit = Map[String, Int]()
    latentLimit += "ROOT" -> 1
    
    val motherSizesToEncode = if(markChildOrder) List(2, 4, 5) else List(2)
  
    for(motherSize <- motherSizesToEncode){
      for(mothersChild <- 1 to motherSize){
        for(perm <- List("A", "N", "P01", "P10", "P12", "P21", "P2413", "P3142", "P24153", "P25314", "P42513", "P41352", "P35142", "P31524")){
          val motherSizeDesc   = if(markChildOrder) motherSize   else 0
          val mothersChildDesc = if(markChildOrder) mothersChild else 0
          if(motherSize>2){
            // How many nonterms that cause combinatorial explosion?
            val nt = "M"+motherSizeDesc+"C"+mothersChildDesc+perm
            latentLimit += nt -> 1
          }else{
            if(perm == "A" || perm == "N"){
              // How many POS tags?
              val nt = "M"+motherSizeDesc+"C"+0+perm
              latentLimit += nt -> 15
            }else{
              // How many nonterms that are binary?
              val nt = "M"+motherSizeDesc+"C"+0+perm
              latentLimit += nt -> 15
            }
          }
        }
      }
    }
    
    latentLimit
  }


  def split(
      oldG:Grammar,
      threads:Int,
      categorySplits:Map[String, Int]=defaultCategorySplitsConfigurationUnmarked
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
    var newGsize = 0
    val parallelOldRules = oldG.allRules.par
    parallelOldRules.tasksupport = 
      new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(threads))
    val newRules:List[Rule] = parallelOldRules.flatMap{
      case rule @ InnerRule(oldLhs, oldRhs, oldProb) =>
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
        
        val result = combinations.map{ case lhs::rhs =>
          var randomness = if(combinationsSize == 1) 0.0 else Random.nextDouble()/100 - 0.005
          while(oldProb.toDouble + randomness > 1.0 || oldProb.toDouble + randomness < 0.0){
            randomness = Random.nextDouble()/100 - 0.005
          }
          InnerRule(lhs, rhs, Probability(oldProb.toDouble + randomness))
        }
        processed += 1
        newGsize += result.size
        if(processed % 10 == 0){
          System.err.println(processed+"/"+oldGsize+" newGsize="+newGsize)
        }
        result
      case rule @ PretermRule(oldLhs, word, prob) =>
        val nonTermString = oldNonTerms(oldLhs)
        val numOfSplits = categorySplits(nonTermString)
        val representation = if(numOfSplits == 1){
                               List(newNonTerms(nonTermString))
                             }else{
                               (0 until numOfSplits).map{ i => newNonTerms(nonTermString+"_"+i) }.toList
                             }
        // System.err.println("num of new latent rules: " + representation.size)
        val result = representation.map{ case lhs =>
          var randomness = if(representation.size == 1) 0.0 else Random.nextDouble()/100 - 0.005
          while(prob.toDouble + randomness > 1.0 || prob.toDouble + randomness < 0.0) {
            randomness = Random.nextDouble()/100 - 0.005
          }
          PretermRule(lhs, word, Probability(prob.toDouble+randomness))
        }
        processed += 1
        newGsize += result.size
        if(processed % 10 == 0){
          System.err.println(processed+"/"+oldGsize+" newGsize="+newGsize)
        }
        result
    }.toList
    System.err.println("finished initial split operation")
    
    val newG = new Grammar(normalize(newRules, threads), newLatentMappings, oldVoc, newNonTerms)
    System.err.println("grammar is created")
    newG
  }

  def smoothSplits(oldG:Grammar, threads:Int) : Grammar = {
    val alpha = 0.01
    val newRules:List[Rule] = oldG.allRules.groupBy{
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
    }

    new Grammar(normalize(newRules, threads), oldG.latentMappings, oldG.voc, oldG.nonTerms)
  }

  private def normalize(rules:List[Rule], threads:Int) : List[Rule] = {
    val normalization = scala.collection.mutable.Map[NonTerm, Double]().withDefaultValue(0.0)
    System.err.println("STARTED normalization")
    val t1 = System.currentTimeMillis()
    
    for(rule <- rules){
      normalization(rule.lhs) = normalization(rule.lhs) + rule.prob.toDouble
    }
    
    var newRules = List[Rule]()
    rules.foreach{
      case InnerRule(lhs, rhs, prob) =>
        newRules ::= InnerRule(lhs, rhs, Probability(prob.toDouble/normalization(lhs)))
      case PretermRule(lhs, word, prob) =>
        newRules ::= PretermRule(lhs, word, Probability(prob.toDouble/normalization(lhs)))
    }
    
    val t2 = System.currentTimeMillis()
    val period = t2 - t1
    System.err.println(s"DONE normalization in $period ms")
    newRules
  }

}
