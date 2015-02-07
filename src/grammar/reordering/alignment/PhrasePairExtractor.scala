package grammar.reordering.alignment

import grammar.reordering.representation.PretermRule
import grammar.reordering.representation.Rule
import grammar.reordering.representation.Probability.LogOne
import grammar.reordering.representation.IntMapping
import grammar.reordering.representation.InnerRule
import grammar.reordering.representation.NonTerm
import grammar.reordering.representation.Grammar

object PhrasePairExtractor {
  
  private type Alignment = Set[(Int, Int)]
  private type PhrasePair = ((Int, Int), (Int, Int))

  def findPhrases(a:Alignment, e_n:Int) : List[PhrasePair] = {
    if(a.size < 2 || e_n < 2){
      return a.map{case (e, f) => ((e, e), (f, f))}.toList
    }
    var res = List[PhrasePair]()
    
    val bestSpans = Array.fill(e_n)((0, e_n-1, (0, 200*e_n)))
    
    for(e_i <- 0 until e_n){
      for(e_j <- e_i until e_n){
        val p = isPhrase(e_i, e_j, a)
        // if(p == (-1, -1)){
        if(p != null){
          for(k <- e_i to e_j){
            if(bestSpans(k)._2 - bestSpans(k)._1 +1 > e_j-e_i+1){
              bestSpans(k) = (e_i, e_j, p)
            }
          }
        }
      }
    }

    var leftToProcess = e_n
    val processed = Array.fill(e_n)(false)
    
    while(leftToProcess != 0){
      var widestMinIndex = -1
      var widestMinValue = -1

      for(i <- 0 until bestSpans.size){
        if(! processed(i)){
          val width = bestSpans(i)._2 - bestSpans(i)._1 + 1
          if(width > widestMinValue){
            widestMinValue = width
            widestMinIndex = i
          }
        }
      }
      
      val e_start = bestSpans(widestMinIndex)._1
      val e_end = bestSpans(widestMinIndex)._2
      val f_start = bestSpans(widestMinIndex)._3._1
      val f_end = bestSpans(widestMinIndex)._3._2
      
      res ::= ((e_start, e_end), (f_start, f_end))
      leftToProcess -= widestMinValue
      for(i <- e_start to e_end){
        processed(i) = true
      }
    }
    
    res
  }
  
  
  private def findPhrases_old(a:Alignment, e_n:Int) : List[PhrasePair] = {
    var res = List[PhrasePair]()
    var e_i = 0
    var done = false

    while(e_i < e_n && ! done){
      var e_j = e_i
      var nextRound = false
      while(e_j < e_n && ! nextRound){
        val p = isPhrase(e_i, e_j, a)
        if(p == (-1, -1)){
          //unaligned word/phrase
          res ::= ((e_i, e_j), p)
          e_i = e_j+1
          nextRound = true
        }else if(p == null){
          e_j += 1
        }else{
          res ::= ((e_i, e_j), p)
          e_i = e_j+1
          nextRound = true
        }
      }
    }
    
    res
  }
  
  def unfoldGrammarOfIdioms(oldG:Grammar) : Grammar = {
    
    val newLatentMappings = oldG.latentMappings 
    val newVoc = oldG.voc.clone()
    val newNonTerms = oldG.nonTerms.clone()

    val newRules = scala.collection.mutable.Set[Rule]()
      
    oldG.allRules.foreach{
      case rule @ InnerRule(_, _, _) =>
        newRules += rule
      case rule @ PretermRule(_, _, _) =>
        for(newRule <- unfakePretermRule(rule, newNonTerms, newVoc)){
          newRules += newRule
        }
    }
    
    val newGrammar = new Grammar(rulesArg = newRules, latentMappings = newLatentMappings, voc = newVoc, nonTerms = newNonTerms )
    
    newGrammar
  }
  
  private def unfakePretermRule(rule:PretermRule, nonTerms:IntMapping, voc:IntMapping) : List[Rule] = {
    val tagMarker = "tag_"
    val lhsStr:String = nonTerms(rule.lhs)
    if(lhsStr.startsWith(tagMarker+"[[[") && lhsStr.endsWith("]]]") && lhsStr.contains("___")){
      var res = List[Rule]()
      val words = lhsStr.drop(7).dropRight(3).split("___").toList.map{_.replaceAllLiterally("STAR", "*")}
      var rhsNodes = List[NonTerm]()
      for(word <- words){
        if( ! voc.contains(word)){

          voc.unlock()
          val wCode = voc(word)
          voc.lock()

          val unknownTag = tagMarker+Grammar.unknownToken
          nonTerms.unlock()
          val ntCode = nonTerms(unknownTag)
          nonTerms.lock()

          res ::= PretermRule(ntCode, wCode, LogOne)
        }
        
        val posTag = tagMarker + word.replaceAllLiterally("*", "STAR")
        nonTerms.unlock()
        val ntCode = nonTerms(posTag)
        nonTerms.lock() // we have to be careful so it's better to lock it

        rhsNodes ::= ntCode
        // voc.unlock()
        val wCode = voc(word)
        // voc.lock()      // we have to be careful so it's better to lock it

        res ::= PretermRule(ntCode, wCode, LogOne)
      }
      rhsNodes = rhsNodes.reverse
      res ::= InnerRule(rule.lhs, rhsNodes, rule.prob)
      res
    }else{
      List(rule)
    }
  }
  
  def unfakePhrase(phrase:String) : List[String] = {
    if(phrase.startsWith("[[[") && phrase.endsWith("]]]")){
      val realPhrase = phrase.substring(3, phrase.size-3)
      realPhrase.split("___").toList
    }else{
      List(phrase)
    }
  }
  
  def fakeAlignmentAndFakeWords(sent:List[String], spans:List[PhrasePair]) :
     (List[String], Set[(Int, Int)]) = {
    val newSent = spans.map{_._1}.sortBy{_._1}.map{ case (e1, e2) =>
      if(e1 == e2){ // not a phrase
        sent(e1)
      }else{
        val mergedPhrase = sent.drop(e1).take(e2-e1+1).mkString("___")
        "[[["+mergedPhrase+"]]]"
      }
    }
    val newAlign = spans.sortBy{_._1._1}.zipWithIndex.flatMap{
      case (((e1, e2), (f1, f2)), index) =>
        if(f1 == -1){
          List()
        }else{
          List((index,f1))
        }
    }.toSet
    (newSent, newAlign)
  }
  
  private def isPhrase(e_i:Int, e_j:Int, a:Alignment) : (Int, Int) = {
    val (e_links, non_e_links) = a.partition{case (e, f) => e_i <= e && e<= e_j}
    
    if(e_links.isEmpty){
      return (-1, -1)
    }
    if( ! e_links.exists{_._1 == e_i}){
      return null
    }
    if( ! e_links.exists{_._1 == e_j}){
      return null
    }

    val f_min = e_links.map{_._2}.min
    val f_max = e_links.map{_._2}.max
    
    if(non_e_links.exists{case (e:Int, f:Int) =>
      f_min <= f && f <= f_max }){
      null
    }else{
      (f_min, f_max)
    }
  }

}