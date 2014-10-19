package grammar.reordering.representation

class Grammar ( rulesArg:Set[Rule],
                val latentMappings:Map[NonTerm,List[NonTerm]],
                val voc:IntMapping,
                val nonTerms:IntMapping,
                val latestSplits:List[(String, String, String, Double, Double)],
                dummy:Boolean=false) {
  
  val latestSplitsInts:List[(NonTerm, NonTerm, NonTerm, Double, Double)] = latestSplits.map{ case (mother:String, split1:String, split2:String, p1Raw:Double, p2Raw:Double) =>
    (nonTerms(mother), nonTerms(split1), nonTerms(split2), p1Raw, p2Raw)
  }
  
  def copyConstructor(newRulesArg:Set[Rule]) : Grammar = {
    new Grammar(newRulesArg, latentMappings, voc, nonTerms, latestSplits, dummy)
  }
  
  val innerRules:Map[(NonTerm, List[NonTerm]), InnerRule] = rulesArg.flatMap{
    case innerRule   @ InnerRule  (lhs, rhs , _) => List((lhs, rhs ) -> innerRule)
    case _ => List()
  }.toMap
    
    
  val pretermRules:Map[(NonTerm, Word), PretermRule] = rulesArg.flatMap{
    case pretermRule @ PretermRule(lhs, word, _) => List((lhs, word) -> pretermRule)
    case _ => List()
  }.toMap
  
  def allRules : Set[Rule] = innerRules.valuesIterator.toSet ++ pretermRules.valuesIterator.toSet
  
  def getInnerRule(lhs:NonTerm, rhs:List[NonTerm]) : Rule = {
    if(dummy){
      InnerRule(lhs, rhs, 0.1)
    }else{
      val representation = (lhs, rhs)
      innerRules(representation)
    }
  }
  
  def getPretermRule(lhs:NonTerm, word:Word) : Rule = {
    if(dummy){
      PretermRule(lhs, word, 0.1)
    }else{
      val representation = (lhs, word)
      pretermRules(representation)
    }
  }
  
  def getAllLatentInnerRules(lhsOriginal:NonTerm, rhsOriginal:List[NonTerm]) : Set[Rule] = {
    for(el <- rhsOriginal){
      if(nonTerms(el).endsWith("_0") || nonTerms(el).endsWith("_1"))
        println(el+ " " + nonTerms(el))
    }
    if(dummy){
      Set(InnerRule(lhsOriginal, rhsOriginal, 0.1))
    }else{
      var rules = Set[Rule]()
      for(lhs <- latentMappings(lhsOriginal)){
        val rhsVersions = Grammar.allCombinations(rhsOriginal.map{latentMappings(_)})

        for(rhs <- rhsVersions){
          val representation = (lhs, rhs)
          if(innerRules contains representation){
            rules += innerRules(representation)
          }
        }
      }
      rules
    }
  }
  
  def getAllLatentPretermRules(lhsOriginal:NonTerm, word:Word) : Set[Rule] = {
    if(dummy){
      Set(PretermRule(lhsOriginal, word, 0.1))
    }else{
      var rules = Set[Rule]()
      for(lhs <- latentMappings(lhsOriginal)){
        val representation = (lhs, word)
        if(pretermRules contains representation){
          rules += pretermRules(representation)
        }
      }
      rules
    }
  }
  
  def getRuleIgnoringProb(rule:Rule) : Rule = {
    if(dummy){
      rule
    }else{
      rule match {
        case InnerRule(lhs, rhs, _) => innerRules((lhs, rhs))
        case PretermRule(lhs, word, _) => pretermRules((lhs, word))
      }
    }
  }
  
  override
  def toString() : String =
    this.allRules.toList.
      sortBy(rule => nonTerms(rule.lhs)).
      map{rule:Rule => rule.toString(voc, nonTerms)}.
      mkString("\n")

}

object Grammar{
  
  def allCombinations[A](input:List[List[A]]) : List[List[A]] = {
    if(input.size == 1)
      input.head.map{List(_)}
    else
      allCombinations(input.tail).flatMap{sequel => input.head.map{_::sequel}}
  }
  
}