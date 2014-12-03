package grammar.reordering.representation

import java.io.PrintWriter
import scala.io.Source
import java.io.File
import java.text.SimpleDateFormat
import java.util.Date

class Grammar ( rulesArg:List[Rule], // scala.collection.Set[Rule],
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
    
  private lazy val rootSum = Probability.sum(rulesArg.toList.filter{_.lhs == ROOT}.map{_.prob})

  System.err.println("STARTED innerRules computation")
  val innerRules:scala.collection.Map[(NonTerm, List[NonTerm]), InnerRule] = computeInnerRules(rulesArg)
  System.err.println("DONE innerRules computation")
  private def computeInnerRules(allRules:Traversable[Rule]) : scala.collection.Map[(NonTerm, List[NonTerm]), InnerRule] = {
    val selectedInnerRules = scala.collection.mutable.Map[(NonTerm, List[NonTerm]), InnerRule]()
    allRules.foreach{
      case innerRule   @ InnerRule  (lhs, rhs , prob) =>
        if(lhs == ROOT){
          val rootRule = InnerRule(lhs, rhs, Probability(1-Grammar.GLUEweight)*prob/rootSum)
          if(rhs.head != ROOT && ! alreadyDefinedROOTinners.contains(rhs)){
            assert(rhs.size == 1)
            val glueRhs = List(ROOT, rhs.head)
            val glueRule = InnerRule(ROOT, glueRhs, Probability(Grammar.GLUEweight)*prob/rootSum)// Probability(Double.MinPositiveValue))
            selectedInnerRules += (ROOT, glueRhs) -> glueRule
            selectedInnerRules += (lhs, rhs ) -> rootRule
          }else{
            selectedInnerRules += (lhs, rhs) -> rootRule
          }
        }else{
          selectedInnerRules += (lhs, rhs ) -> innerRule
        }
      case _ =>
    }
    selectedInnerRules
  }
    
  System.err.println("STARTED pretermRules computation")
  val pretermRules:scala.collection.Map[(NonTerm, Word), PretermRule] = computePretermRules(rulesArg)
  System.err.println("DONE pretermRules computation")
  private def computePretermRules(allRules:Traversable[Rule]) : scala.collection.Map[(NonTerm, Word), PretermRule] = {
    val selectedPretermRules = scala.collection.mutable.Map[(NonTerm, Word), PretermRule]()

    allRules.foreach{
      case pretermRule @ PretermRule(lhs, word, _) => selectedPretermRules += (lhs, word) -> pretermRule
      case _ => 
    }

    selectedPretermRules
  }
  
  lazy val allRules:Traversable[Rule] = innerRules.valuesIterator.toTraversable ++ pretermRules.valuesIterator.toTraversable
  
  def getInnerRule(lhs:NonTerm, rhs:List[NonTerm]) : Rule = {
    if(dummy){
      InnerRule(lhs, rhs, Probability(0.1))
    }else{
      val representation = (lhs, rhs)
      innerRules(representation)
    }
  }
  
  lazy val pretermRulesForWord:scala.collection.Map[Word, List[PretermRule]] = computePretermRulesForWord(pretermRules.values)
  private def computePretermRulesForWord(allRules:Traversable[PretermRule]) : scala.collection.Map[Word, List[PretermRule]] = {
    val selectedPretermRules = scala.collection.mutable.Map[Word, List[PretermRule]]().withDefaultValue(List())

    allRules.foreach{ case rule @ PretermRule(_, word, _) =>
      selectedPretermRules += word -> (rule::selectedPretermRules(word))
    }

    selectedPretermRules
  }

  lazy val innerUnaryRulesForLeftmostNonTerm:scala.collection.Map[NonTerm, List[InnerRule]] = computeUnaryRulesForLeftmostNonTerm(innerRules.values)
  private def computeUnaryRulesForLeftmostNonTerm(allRules:Traversable[InnerRule]) : scala.collection.Map[NonTerm, List[InnerRule]] = {
    val selectedInnerRules = scala.collection.mutable.Map[NonTerm, List[InnerRule]]().withDefaultValue(List())
    
    allRules.foreach{ case rule @ InnerRule(_, rhs, _) =>
      if(rhs.size == 1){
        selectedInnerRules += rhs.head -> (rule::selectedInnerRules(rhs.head))
      }
    }
    
    selectedInnerRules
  }
  
  lazy val innerNaryRulesForLeftmostNonTerm:scala.collection.Map[NonTerm, List[InnerRule]] = computeNaryRulesForLeftmostNonTerm(innerRules.values)
  private def computeNaryRulesForLeftmostNonTerm(allRules:Traversable[InnerRule]) : scala.collection.Map[NonTerm, List[InnerRule]] = {
    val selectedInnerRules = scala.collection.mutable.Map[NonTerm, List[InnerRule]]().withDefaultValue(List())
    
    allRules.foreach{ case rule @ InnerRule(_, rhs, _) =>
      if(rhs.size > 1){
        selectedInnerRules += rhs.head -> (rule::selectedInnerRules(rhs.head))
      }
    }
    
    selectedInnerRules
  }
  
  private lazy val optimizedLatentInnerRulesQuery:scala.collection.Map[List[NonTerm], List[Rule]] = computeOptimizedLatentInnerRulesQuery(innerRules.values)
  private def computeOptimizedLatentInnerRulesQuery(allRules : Traversable[InnerRule]) : scala.collection.Map[List[NonTerm], List[Rule]] = {
    System.err.println("STARTED optimizing latent InnerRules")
    val t1 = System.currentTimeMillis()
    val selectedInnerRules = scala.collection.mutable.Map[List[NonTerm], List[Rule]]().withDefaultValue(List())
    allRules.foreach{ case rule @ InnerRule(lhs, rhs, prob) =>
      val representation = (lhs::rhs).map{reverseLatentMappings(_)}
      selectedInnerRules += representation -> (rule :: selectedInnerRules(representation))
    }
    val t2 = System.currentTimeMillis()
    val period = t2 - t1
    System.err.println(s"DONE optimizing latent InnerRules in $period ms")
    selectedInnerRules
  }
    
  private lazy val optimizedLatentPretermRulesQuery:scala.collection.Map[(NonTerm, Word), List[Rule]] = computeOptimizedLatentPretermRulesQuery(pretermRules.values)
  private def computeOptimizedLatentPretermRulesQuery(allRules:Traversable[PretermRule]) : scala.collection.Map[(NonTerm, Word), List[Rule]] = {
    System.err.println("STARTED optimizing latent PretermRules")
    val t1 = System.currentTimeMillis()
    val selectedPretermRules = scala.collection.mutable.Map[(NonTerm, Word), List[Rule]]().withDefaultValue(List())
    allRules.foreach{ case rule @ PretermRule(lhs, word, prob) =>
      val representation = (reverseLatentMappings(lhs), word)
      selectedPretermRules += representation -> (rule :: selectedPretermRules(representation))
    }
    val t2 = System.currentTimeMillis()
    val period = t2 - t1
    System.err.println(s"DONE optimizing latent PretermRules in $period ms")
    selectedPretermRules
  }
    
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
  
  def save(fnOut:String) : Unit = {
    val fn = if(new File(fnOut).exists()){
      System.err.println(s"$fnOut already exists!")

      val ft = new SimpleDateFormat ("_HH:mm_dd.MM.yyyy")
      val newFN = fnOut+ft.format(new Date())
      System.err.println(s"I'll save to $newFN instead")
      newFN
    }else{
      fnOut
    }
    System.err.println(s"STARTED saving the grammar at $fn")
    val pw = new PrintWriter(fn)
    
    pw.println("NONTERMS ||| "+nonTerms.allStrings.mkString(" "))
    
    rulesArg.foreach{
      case InnerRule(lhs, rhs, prob) =>
        val lhsStr  = nonTerms(lhs)
        val rhsStr  = rhs.map{nonTerms(_)}.mkString(" ")
        val probStr = prob.toDouble
        if(prob.toDouble != 0.0){
          pw.println(s"RULE ||| $lhsStr -> $rhsStr ||| $probStr")
        }
      case PretermRule(lhs, word, prob) =>
        val lhsStr   = nonTerms(lhs)
        val wordStr  = voc(word)
        val probStr = prob.toDouble
        if(prob.toDouble != 0.0){
          pw.println(s"RULE ||| $lhsStr -> '$wordStr' ||| $probStr")
        }
    }
    
    latentMappings.toList.map{ case (mother, children) =>
      val motherStr = nonTerms(mother)
      val childrenStr = children.map{nonTerms(_)}.mkString(" ")
      s"SPLIT ||| $motherStr ||| $childrenStr"
    }.sorted.foreach{pw.println(_)}

    pw.close()

    System.err.println(s"DONE saving the grammar at $fn")

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
    var rulesArg = List[Rule]()
    
    Source.fromFile(fn).getLines().foreach{
      case splitRx(motherStr, splitsStr) =>
        latentMappings += nonTerms(motherStr) -> splitsStr.split(" +").toList.map{nonTerms(_)}
      case nonTermsRx(nonTermsStr) =>
        nonTermsStr.split(" +").foreach{nonTerms(_)}
      case pretermRuleRx(lhsStr, wordStr, prob) =>
        rulesArg ::= PretermRule(nonTerms(lhsStr), voc(wordStr), Probability(prob.toDouble))
      case generalRuleRx(lhsStr, rhsStr, prob) =>
        rulesArg ::= InnerRule(nonTerms(lhsStr), rhsStr.split(" +").toList.map{nonTerms(_)}, Probability(prob.toDouble))
    }
    
    new Grammar(rulesArg, latentMappings, voc, nonTerms)
  }
  
}