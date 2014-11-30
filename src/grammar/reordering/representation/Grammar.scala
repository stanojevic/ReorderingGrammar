package grammar.reordering.representation

import java.io.PrintWriter
import scala.io.Source

class Grammar ( rulesArg:Set[Rule],
                val latentMappings:Map[NonTerm,List[NonTerm]],
                val voc:IntMapping,
                val nonTerms:IntMapping,
                dummy:Boolean=false) {

  val unknown : Word = voc(Grammar.unknownToken)
  val ROOT : NonTerm = nonTerms(Grammar.ROOTtoken)
  
  if(! dummy){
    voc.lock()
    nonTerms.lock()
  }
  
  val reverseLatentMappings:Map[NonTerm, NonTerm] = latentMappings.flatMap{
    case (parent:NonTerm, latents:List[NonTerm]) => latents.map{(_, parent)}
  }
  
  private lazy val permRx = """.*P(\d+).*$""".r
  lazy val permutationMappings:Map[NonTerm, List[Int]] = nonTerms.allInts.map{ case nonTerm =>
    val p = nonTerms(nonTerm) match {
      case "ROOT" => List(0, 0)
      case permRx(perm) => perm.split("").toList.tail.map{_.toInt}
      case _ => List()
    }
    (nonTerm, p)
  }.toMap
  
  private lazy val alreadyDefinedROOTinners = rulesArg.filter{ rule =>
    rule.isInstanceOf[InnerRule] &&
    rule.lhs == ROOT             &&
    rule.asInstanceOf[InnerRule].rhs.head == ROOT
    }.map{_.asInstanceOf[InnerRule].rhs}.toSet
    
  private val rootSum = Probability.sum(rulesArg.toList.filter{_.lhs == ROOT}.map{_.prob})

  val innerRules:Map[(NonTerm, List[NonTerm]), InnerRule] = rulesArg.flatMap{
    case innerRule   @ InnerRule  (lhs, rhs , prob) =>
      if(lhs == ROOT){
        val rootRule = InnerRule(lhs, rhs, Probability(1-Grammar.GLUEweight)*prob/rootSum)
        if(rhs.head != ROOT && ! alreadyDefinedROOTinners.contains(rhs)){
          assert(rhs.size == 1)
          val glueRhs = List(ROOT, rhs.head)
          val glueRule = InnerRule(ROOT, glueRhs, Probability(Grammar.GLUEweight)*prob/rootSum)// Probability(Double.MinPositiveValue))
          List(
              (ROOT, glueRhs) -> glueRule,
              (lhs, rhs ) -> rootRule
              )
        }else{
          List((lhs, rhs) -> rootRule)
        }
      }else{
        List(
            (lhs, rhs ) -> innerRule
            )
      }
    case _ =>
      List()
  }.toMap
    
    
  val pretermRules:Map[(NonTerm, Word), PretermRule] = rulesArg.flatMap{
    case pretermRule @ PretermRule(lhs, word, _) => List((lhs, word) -> pretermRule)
    case _ => List()
  }.toMap
  
  lazy val allRules:Traversable[Rule] = innerRules.valuesIterator.toTraversable ++ pretermRules.valuesIterator.toTraversable
  
  def getInnerRule(lhs:NonTerm, rhs:List[NonTerm]) : Rule = {
    if(dummy){
      InnerRule(lhs, rhs, Probability(0.1))
    }else{
      val representation = (lhs, rhs)
      innerRules(representation)
    }
  }
  
  val pretermRulesForWord:Map[Word, List[PretermRule]] = pretermRules.values.groupBy{case PretermRule(_, word, _) => word}.mapValues{_.toList}.withDefaultValue(List())

  val innerUnaryRulesForLeftmostNonTerm:Map[NonTerm, List[InnerRule]] = innerRules.values.
  filter{
    case InnerRule(_, rhs, _) => rhs.size == 1
  }.
  groupBy{
    case InnerRule(_, List(rhs1, _*), _) => rhs1
  }.mapValues{_.toList}.withDefaultValue(List())
  
  val innerNaryRulesForLeftmostNonTerm:Map[NonTerm, List[InnerRule]] = innerRules.values.
  filter{
    case InnerRule(_, rhs, _) => rhs.size > 1
  }.
  groupBy{
    case InnerRule(_, List(rhs1, _*), _) => rhs1
  }.mapValues{_.toList}.withDefaultValue(List())
  
  private val optimizedLatentInnerRulesQuery:Map[List[NonTerm], List[Rule]] = innerRules.values.groupBy{ case InnerRule(lhs, rhs, prob) =>
    (lhs::rhs).map{reverseLatentMappings(_)}
  }.map{case (groupId, rules) => (groupId, rules.toList.asInstanceOf[List[Rule]])}

  private val optimizedLatentPretermRulesQuery:Map[(NonTerm, Word), List[Rule]] = pretermRules.values.groupBy{ case PretermRule(lhs, word, prob) =>
    (reverseLatentMappings(lhs), word)
  }.map{case (groupId, rules) => (groupId, rules.toList.asInstanceOf[List[Rule]])}
  
  def getPretermRule(lhs:NonTerm, word:Word) : Rule = {
    if(dummy){
      PretermRule(lhs, word, Probability(0.1))
    }else{
      val representation = (lhs, word)
      pretermRules(representation)
    }
  }
  
  def getAllLatentInnerRules(lhsOriginal:NonTerm, rhsOriginal:List[NonTerm]) : List[Rule] = {
    if(dummy){
      List(InnerRule(lhsOriginal, rhsOriginal, Probability(0.1)))
    }else{
      optimizedLatentInnerRulesQuery(lhsOriginal::rhsOriginal)
    }
  }
  
  def getAllLatentPretermRules(lhsOriginal:NonTerm, word:Word) : List[Rule] = {
    if(dummy){
      List(PretermRule(lhsOriginal, word, Probability(0.1)))
    }else{
      optimizedLatentPretermRulesQuery((lhsOriginal,word))
    }
  }
  
  def save(fn:String) : Unit = {
    val pw = new PrintWriter(fn)
    
    pw.println("NONTERMS ||| "+nonTerms.allStrings.mkString(" "))
    
    rulesArg.toList.flatMap{
      case InnerRule(lhs, rhs, prob) =>
        val lhsStr  = nonTerms(lhs)
        val rhsStr  = rhs.map{nonTerms(_)}.mkString(" ")
        val probStr = prob.toDouble
        if(prob.toDouble == 0.0){
          List()
        }else{
          List(s"RULE ||| $lhsStr -> $rhsStr ||| $probStr")
        }
      case PretermRule(lhs, word, prob) =>
        val lhsStr   = nonTerms(lhs)
        val wordStr  = voc(word)
        val probStr = prob.toDouble
        if(prob.toDouble == 0.0){
          List()
        }else{
          List(s"RULE ||| $lhsStr -> '$wordStr' ||| $probStr")
        }
    }.sorted.foreach{pw.println(_)}
    
    latentMappings.toList.map{ case (mother, children) =>
      val motherStr = nonTerms(mother)
      val childrenStr = children.map{nonTerms(_)}.mkString(" ")
      s"SPLIT ||| $motherStr ||| $childrenStr"
    }.sorted.foreach{pw.println(_)}

    pw.close()
  }
  
}

object Grammar{
  
  val unknownToken = "XXX_UNKNOWN_XXX"
  val ROOTtoken = "ROOT"
    
  val GLUEweight = 0.00001
  
  def allCombinations[A](input:List[List[A]]) : List[List[A]] = {
    if(input.size == 1)
      input.head.map{List(_)}
    else
      allCombinations(input.tail).flatMap{sequel => input.head.map{_::sequel}}
  }
  
  def loadFromFile(fn:String) : Grammar = {
    val splitRx       = """^SPLIT \|\|\| (.+) \|\|\| (.+)""".r
    val pretermRuleRx = """^RULE \|\|\| (.*) -> '(.*)' \|\|\| (.*)""".r
    val generalRuleRx = """^RULE \|\|\| (.*) -> (.*) \|\|\| (.*)""".r
    val nonTermsRx    = """^NONTERMS \|\|\| (.+)""".r
    
    val nonTerms = new IntMapping()
    val voc = new IntMapping()
    var latentMappings = Map[NonTerm, List[NonTerm]]()
    var rulesArg = Set[Rule]()
    
    Source.fromFile(fn).getLines().foreach{
      case splitRx(motherStr, splitsStr) =>
        latentMappings += nonTerms(motherStr) -> splitsStr.split(" +").toList.map{nonTerms(_)}
      case nonTermsRx(nonTermsStr) =>
        nonTermsStr.split(" +").foreach{nonTerms(_)}
      case pretermRuleRx(lhsStr, wordStr, prob) =>
        rulesArg += PretermRule(nonTerms(lhsStr), voc(wordStr), Probability(prob.toDouble))
      case generalRuleRx(lhsStr, rhsStr, prob) =>
        rulesArg += InnerRule(nonTerms(lhsStr), rhsStr.split(" +").toList.map{nonTerms(_)}, Probability(prob.toDouble))
    }
    
    new Grammar(rulesArg, latentMappings, voc, nonTerms)
  }
  
}