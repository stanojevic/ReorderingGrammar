package grammar.reordering.representation

class Grammar ( rulesArg:Set[Rule],
                val latentMappings:Map[NonTerm,Set[NonTerm]],
                val voc:IntMapping,
                val nonTerms:IntMapping,
                dummy:Boolean=false) {
  
  var locked = false
  
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
  
  private def allCombinations[A](input:List[Set[A]]) : Set[List[A]] = {
    if(input.size == 1)
      input.head.map{List(_)}
    else
      allCombinations(input.tail).flatMap{sequel => input.head.map{_::sequel}}
  }
  
  def getAllLatentInnerRules(lhsOriginal:NonTerm, rhsOriginal:List[NonTerm]) : Set[Rule] = {
    if(dummy){
      Set(InnerRule(lhsOriginal, rhsOriginal, 0.1))
    }else{
      var rules = Set[Rule]()
      for(lhs <- latentMappings(lhsOriginal)){
        val rhsVersions = allCombinations(rhsOriginal.map{latentMappings(_)})

        for(rhs <- rhsVersions){
          val representation = (lhs, rhs)
          rules += innerRules(representation)
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
        rules += pretermRules(representation)
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

}