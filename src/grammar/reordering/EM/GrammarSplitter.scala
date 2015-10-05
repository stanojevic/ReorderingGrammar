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
  
  val defaultCategorySplitsConfiguration : Map[String, Int] = computeDefaultCategorySplitsConfiguration(10, 1)
  
  def computeDefaultCategorySplitsConfiguration(binarySplits:Int, nArySplits:Int) : Map[String, Int] = {
    var latentLimit = Map[String, Int]().withDefaultValue(1)
    latentLimit += "ROOT" -> 1
    
    val arity1nonTerms = Set("A", "N")
    val arity2nonTerms = Set("P01", "P10", "P12", "P21")
    val arity4nonTerms = Set("P2413", "P3142")
    val arity5nonTerms = Set("P24153", "P25314", "P42513", "P41352", "P35142", "P31524")
  
    for(perm <- arity1nonTerms ++ arity2nonTerms ++ arity4nonTerms ++ arity5nonTerms){
      val arity = if(perm.size == 1) 1 else perm.size-1
      if(arity1nonTerms.contains(perm)){
        // How many POS tags?
        val nt = perm
        latentLimit += nt -> 3
      }else{
        val nt = perm
        val splitNumber:Int = if(arity == 2) binarySplits else nArySplits
        latentLimit += nt -> splitNumber
        for(i <- 1 to arity){
          latentLimit += s"$nt*$i" -> splitNumber
        }
      }
    }
    
    latentLimit
  }

  def split(
      oldG:Grammar,
      threads:Int,
      categorySplits:Map[String, Int]=defaultCategorySplitsConfiguration
      ) : Grammar = {
    
    val oldVoc = oldG.voc
    val oldNonTerms = oldG.nonTerms
    val oldLatentMappings = oldG.latentMappings
    
    val newNonTerms = new IntMapping()

    var newLatentMappings = Map[NonTerm, List[NonTerm]]()
    for(motherNonTermString <- oldNonTerms.allStrings){
      val splits = categorySplits(motherNonTermString)
      if(splits == 1){
        newLatentMappings += newNonTerms(motherNonTermString) -> List(newNonTerms(motherNonTermString))
      }else{
        newLatentMappings += newNonTerms(motherNonTermString) -> (0 until splits).map{ i => newNonTerms(motherNonTermString+"_"+i) }.toList
      }
    }
    
    var processed = 0
    val oldGsize = oldG.allRules.size
    var newGsize = 0
    
    var newRules = List[Rule]()
    
    oldG.allRules.foreach{
      case oldRule @ InnerRule(oldLhs, oldRhss, oldProb) =>

        val oldLhsStr = oldNonTerms(oldLhs)
        val numOfLhsSplits = categorySplits(oldLhsStr)
        val newLhssStr = if(numOfLhsSplits == 1){
          List(oldLhsStr)
        }else{
          (0 until numOfLhsSplits).map{ i => oldLhsStr+"_"+i}
        }
        val lhsOptions = newLhssStr.map{newNonTerms(_)}

        val arity = oldRhss.size
        if(arity == 1){
          val oldRhs = oldRhss.head
          val oldRhsStr = oldNonTerms(oldRhs)
          val numOfRhsSplits = categorySplits(oldRhsStr)
          val newRhssStr = if(numOfRhsSplits == 1){
            List(oldRhsStr)
          }else{
            (0 until numOfRhsSplits).map{ i => oldRhsStr+"_"+i}
          }
          val rhsOptions = newRhssStr.map{newNonTerms(_)}
          val combinationsSize = lhsOptions.size * rhsOptions.size
          for(lhs <- lhsOptions){
            for(rhs <- rhsOptions){
              var randomness = if(combinationsSize == 1) 0.0 else Random.nextDouble()/100 - 0.005
              while(oldProb.toDouble + randomness > 1.0 || oldProb.toDouble + randomness < 0.0){
                randomness = Random.nextDouble()/100 - 0.005
              }
              val newProb = Probability(oldProb.toDouble + randomness)
              val newRule = InnerRule(lhs, List(rhs), newProb)
              newRules ::= newRule
            }
          }
        }else if(oldLhsStr == Grammar.ROOTtoken){
          // arity > 1
          // && we don't want to split glue rules
          // i know how the rule looks like
          val combinationsSize = lhsOptions.size
          for(lhs <- lhsOptions){
            val lhsStr = newNonTerms(lhs)
            val parts = lhsStr.split("_")
            val newRhssStr = if(parts.size > 1){
              (1 to arity).map{ i => parts(0)+"*"+i+"_"+parts(1) }
            }else{
              (1 to arity).map{ i => parts(0)+"*"+i }
            }
            var randomness = if(combinationsSize == 1) 0.0 else Random.nextDouble()/100 - 0.005
            while(oldProb.toDouble + randomness > 1.0 || oldProb.toDouble + randomness < 0.0){
              randomness = Random.nextDouble()/100 - 0.005
            }
            val newProb = Probability(oldProb.toDouble + randomness)
            val newRhss = newRhssStr.map{newNonTerms(_)}.toList
            val newRule = InnerRule(lhs, newRhss, newProb)
            newRules ::= newRule
          }
        }
      case PretermRule(oldLhs, oldWord, oldProb) =>

        val oldLhsStr = oldNonTerms(oldLhs)
        val numOfLhsSplits = categorySplits(oldLhsStr)
        val lhsOptions = if(numOfLhsSplits == 1){
          List(newNonTerms(oldLhsStr))
        }else{
          (0 until numOfLhsSplits).map{ i => newNonTerms(oldLhsStr+"_"+i)}
        }

        val combinationsSize = lhsOptions.size
        for(lhs <- lhsOptions){
          var randomness = if(combinationsSize == 1) 0.0 else Random.nextDouble()/100 - 0.005
          while(oldProb.toDouble + randomness > 1.0 || oldProb.toDouble + randomness < 0.0){
            randomness = Random.nextDouble()/100 - 0.005
          }
          val newProb = Probability(oldProb.toDouble + randomness)
          val newRule = PretermRule(lhs, oldWord, newProb)
          newRules ::= newRule
        }
    }
    
    System.err.println("finished initial split operation")
    
    val newG = new Grammar(normalize(newRules, threads), newLatentMappings, oldVoc, newNonTerms)
    System.err.println("grammar is created")
    newG
  }
    

  def split_old(
      oldG:Grammar,
      threads:Int,
      categorySplits:Map[String, Int]=defaultCategorySplitsConfiguration
      ) : Grammar = {
    
    val oldVoc = oldG.voc
    val oldNonTerms = oldG.nonTerms
    val oldLatentMappings = oldG.latentMappings
    
    val newNonTerms = new IntMapping()

    var newLatentMappings = Map[NonTerm, List[NonTerm]]()
    for(motherNonTermString <- oldNonTerms.allStrings){
      val splits = categorySplits(motherNonTermString)
    // for((motherNonTermString, splits) <- categorySplits){
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
        
        val result = combinations.map{
          case lhs::rhs =>
            var randomness = if(combinationsSize == 1) 0.0 else Random.nextDouble()/100 - 0.005
            while(oldProb.toDouble + randomness > 1.0 || oldProb.toDouble + randomness < 0.0){
              randomness = Random.nextDouble()/100 - 0.005
            }
            InnerRule(lhs, rhs, Probability(oldProb.toDouble + randomness))
          case _ =>
            throw new Exception("apocalipse")
        }
        processed += 1
        newGsize += result.size
        if(processed % 10000 == 0){
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
