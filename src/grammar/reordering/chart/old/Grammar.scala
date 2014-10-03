package grammar.reordering.chart.old

import scala.io.Source
import Rule.{Node, NonTerm, Term}

class Grammar (
  val lexicon : Set[String],
  val rawRules   : Set[Rule]
) {
  val unaryClosures = Grammar.makeUnaryClosures(rawRules)
  val nAryRules = rawRules filter {
    case Rule(_, List(_), _) => false
    case _                      => true
  }
  
  private val basicPreterminalDistribution = Grammar.makeBasicPreterminalDistribution(rawRules)
  
  def unknownWordRules(word:String) : Set[Rule] =
    basicPreterminalDistribution.map{ case (tag, prob) =>
      Rule(NonTerm(tag), List(Term(word)), prob)
    }.toSet
  
}

object Grammar{
  
  def makeBasicPreterminalDistribution(rules : Set[Rule]) : Map[String, Double] = {
    var dist = scala.collection.mutable.Map[String, Double]()
    var lexicon = Set[String]()
    rules.foreach {
      case Rule(NonTerm(tag), List(Term(word)), prob:Double) =>
        val accumulatedProb:Double = dist.getOrElse(tag, 0.0) + prob
        dist += tag -> accumulatedProb
        lexicon += word
      case _ =>
    }
    
    val n = lexicon.size.toDouble
    
    dist.mapValues(_/n).toMap
  }
  
  /**
   * this approach harms n-best extraction, but
   * it doesn't harm 1-best (Viterbi) nor training (because it's not used there)
   */
  def makeUnaryClosures(rules: Set[Rule]) : Set[UnaryClosureRule] = {
    val originalUnarys = rules filter {rule =>
      rule.rhs match{
        case List(Rule.NonTerm(_)) => true
        case _                     => false
      }
    }
    
    var closures = Map[(Rule.NonTerm, Rule.NonTerm), UnaryClosureRule]()
    for(originalRule @ Rule(left, List(right @ NonTerm(_)), prob) <- originalUnarys){
      val closure = new UnaryClosureRule(left, right, prob, List(originalRule))
      closures += (left, right) -> closure
    }

    for(iteration <- 0 until originalUnarys.size){
      for(rule @ Rule(leftRule, List(rightRule), prob) <- originalUnarys){
        for(((leftClosure, rightClosure), closure) <- closures){
          if(rightRule == leftClosure){
            val currentBestClosure = closures((leftRule, rightClosure))
            val newClosureProb = closure.prob  * rule.prob 
            if(currentBestClosure.prob < newClosureProb){
              val newBestClosure = new UnaryClosureRule(leftRule, rightClosure, newClosureProb, rule::closure.chain)
              closures += (leftRule, rightClosure) -> newBestClosure
            }
          }
        }
      }
    }
    
    closures.values.toSet
  }
  
  
  
  def fromFile(fn:String) : Grammar ={

    val terminalRx = """"([^"]+)"""".r
    val nonTerminalRx = """([^"]+)""".r
    val arrowRx = """->""".r
    val numberRx = """(\d+\.?\d*)""".r
  
    var rules = Set[Rule]()
    var lexicon = Set[String]()

    Source.fromFile(fn).getLines.foreach{ line =>
      val fields = line split " +" toList

      val lhs = fields(0) match {
        case nonTerminalRx(x) => NonTerm(x)
      }
      
      fields(1) match {case arrowRx() =>}

      val rhs = fields.drop(2).init map {
        case terminalRx(x) => {
          lexicon += x
          Term(x)
        }
        case nonTerminalRx(x) => {
          NonTerm(x)
        }
      }
      
      val prob = fields.last match {
        case numberRx(x) => x.toDouble
      }
      
      val rule = new Rule(lhs, rhs, prob)
      rules += rule
    }
    
    new Grammar(lexicon, rules)
  }
}
