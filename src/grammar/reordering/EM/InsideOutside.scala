package grammar.reordering.EM

import grammar.reordering.representation._
import grammar.reordering.representation.ProbabilityHelper.{LogNil, LogOne}
import grammar.reordering.representation.ProbabilityHelper._

object InsideOutside {
  
  def extractAllRules(chart:Chart) : Set[Rule] = {
    val n = chart.size
    var rules = Set[Rule]()
    for(span <- 1 to n){
      for(i <- 0 until n-span+1){
        val j = i + span - 1
        chart(i)(j).values.foreach{ nonTermSpan =>
          rules ++= nonTermSpan.edges.map{_.rule}.toSet
        }
      }
    }
    
    rules
  }
  
  def inside(chart:Chart, nonTerms:IntMapping) : Unit = {
    val n = chart.size
    
    for(i <- 0 until n){
      chart(i)(i).values.foreach{ nonTermSpan =>
        nonTermSpan.inside = logSumExp(nonTermSpan.edges.map{_.rule.logProb})
        nonTermSpan.edges.foreach{ edge =>
          edge.inside = edge.rule.logProb
        }
      }
    }
    
    for(span <- 2 to n){
      for(i <- 0 until n-span+1){
        val j = i + span - 1
        
        val nonTermSpansWithROOT = chart(i)(j).filter(_._1 == nonTerms("ROOT")).values.toList
        val nonTermSpansWithoutROOT = chart(i)(j).filterNot(_._1 == nonTerms("ROOT")).values.toList
        for(nonTermSpan <- nonTermSpansWithoutROOT ++ nonTermSpansWithROOT){
          val lhs = nonTermSpan.edges.head.rule.lhs
          for(edge <- nonTermSpan.edges){
            edge.inside = edge.rule.logProb + 
                           children(edge).map{ case (start:Int, end:Int, childNonTerm:NonTerm) =>
                             chart(start)(end)(childNonTerm).inside
                           }.sum
          }
          chart(i)(j)(lhs).inside = logSumExp(chart(i)(j)(lhs).edges.map{_.inside})
        }
      }
    }
  }
  
  def outside(chart:Chart, nonTerms:IntMapping) : Unit = {
    val n = chart.size
    
    for(i <- 0 until n){
      for(j <- 0 until n){
        chart(i)(j).values.foreach(_.outside = LogNil)
      }
    }
    chart(0)(n-1)(nonTerms("ROOT")).outside = LogOne
    
    for(span <- n to 2 by -1){
      for(i <- 0 until n-span+1){
        val j = i + span - 1
        
        val nonTermSpansWithROOT = chart(i)(j).filter(_._1 == nonTerms("ROOT")).values.toList
        val nonTermSpansWithoutROOT = chart(i)(j).filterNot(_._1 == nonTerms("ROOT")).values.toList
        for(nonTermSpan <- nonTermSpansWithROOT ++ nonTermSpansWithoutROOT){
          for(edge <- nonTermSpan.edges){
            children(edge).map{ case (start:Int, end:Int, childNonTerm:NonTerm) =>
              val outsideAddition =  nonTermSpan.outside + edge.inside - chart(start)(end)(childNonTerm).inside
              chart(start)(end)(childNonTerm).outside = logSumExp(chart(start)(end)(childNonTerm).outside, outsideAddition)
            }
          }
        }
      }
    }
  }
  
  def iteration( sents:List[String],
                 alignments:List[String],
                 g:Grammar,
                 batchSize:Int,
                 parallel:Boolean
                    ) : (Grammar, Double) = {
    val (expectedCounts, mergeLikelihood, allSplitLikelihood) = expectation(sents, alignments, g, batchSize, parallel)
    val newGrammar = maximization(g, expectedCounts)
    (newGrammar, allSplitLikelihood)
  }
  
  def expectation( sents:List[String],
                 alignments:List[String],
                 g:Grammar,
                 batchSize:Int,
                 parallel:Boolean
                    ) : (Map[Rule, Double], Map[(NonTerm, NonTerm, NonTerm), Double], Double) = {
    g.voc.lock()
    val trainingBatches = if(parallel)
                           (sents zip alignments).grouped(batchSize).toList.par
                         else
                           (sents zip alignments).grouped(batchSize).toList

    val  mergeLikelihood2:Map[(NonTerm, NonTerm, NonTerm), Double] = null

    val (
        expectedCounts:Map[Rule, Double],
        mergeLikelihood:Map[(NonTerm, NonTerm, NonTerm), LogProbability],
        allSplitLikelihood:LogProbability) = trainingBatches.map{ miniBatch:List[(String, String)] =>
      miniBatch.map{ case (sent, alignment) =>

        val a = AlignmentCanonicalParser.extractAlignment(alignment)
        val s = sent.split(" +").toList
        s.foreach(g.voc(_)) // just checking whether every word is in vocabulary
  
        val chart = AlignmentForestParser.parse(s, a, g)
        this.inside(chart, g.nonTerms)
        this.outside(chart, g.nonTerms)
        val chartExpectations = this.computeExpectedCountPerChart(chart, g.nonTerms)
        val mergeLikelihood = this.computeMergeLikelihood(chart, g)
        val n = chart.size
        val sentProb = chart(0)(n-1)(g.nonTerms("ROOT")).inside

        (chartExpectations, mergeLikelihood, sentProb)

      }.reduce( sumExpectations )
    }.reduce( sumExpectations )
    
    (expectedCounts, mergeLikelihood, allSplitLikelihood)
  }
  
  def maximization(g:Grammar, expectedCounts:Map[Rule, Double]) : Grammar = {
    // println("START")
    for((rule, expect) <- expectedCounts.toList.sortBy{_._1.lhs }.toList){
      // println(rule.toString(g.voc, g.nonTerms)+ " "+expect)
    }
    // println("DONE")
    
    val newRules:Set[Rule] = g.allRules.groupBy(_.lhs).flatMap{case (lhs:NonTerm, rules:Set[Rule]) =>
      val lhsExpectedCount:Double = rules.toList.map{ rule =>
        expectedCounts(rule)
      }.sum
      rules.map{
        case rule:InnerRule   => rule.copy(p = expectedCounts(rule)/lhsExpectedCount)
        case rule:PretermRule => rule.copy(p = expectedCounts(rule)/lhsExpectedCount)
      }
    }.toSet
    
    val newGrammar = new Grammar(rulesArg = newRules, latentMappings = g.latentMappings, voc = g.voc, nonTerms = g.nonTerms, latestSplits = g.latestSplits )
    
    newGrammar
  }
  
  private def sumExpectations(
      a:(Map[Rule, Double], Map[(NonTerm, NonTerm, NonTerm), LogProbability], LogProbability),
      b:(Map[Rule, Double], Map[(NonTerm, NonTerm, NonTerm), LogProbability], LogProbability))
      : (Map[Rule, Double], Map[(NonTerm, NonTerm, NonTerm), LogProbability], LogProbability) = { 
    val (aExp, aMerge, aLikelihood) = a
    val (bExp, bMerge, bLikelihood) = b
    val cExp = (aExp.keySet ++ bExp.keySet).map{key => key -> (aExp.getOrElse(key, 0.0) + bExp.getOrElse(key, 0.0))}.toMap
    val cMerge = (aMerge.keySet ++ bMerge.keySet).map{key => key -> (aMerge.getOrElse(key, 0.0) + bMerge.getOrElse(key, 0.0))}.toMap
    val cLikelihood = aLikelihood + bLikelihood
    (cExp, cMerge, cLikelihood)
  }
  
  /**
   * MUST NOT be parallelized because it contains mutable vocabulary (and maybe some other things)
   * anyway this phase is really fast so again NO NEED for parallelization
   */
  def initialIteration( sents:List[String],
                        alignments:List[String]
                           ) : Grammar = {
    val voc = new IntMapping()
    var allRules = Set[Rule]()
    val g = new Grammar(
        rulesArg = Set(),
        latentMappings = AlignmentForestParser.defaultLatentMappings,
        nonTerms = AlignmentForestParser.defaultNonTerms,
        voc = voc,
        latestSplits = List(),
        dummy=true
        )
    (sents zip alignments).foreach{ case (sent, alignment) =>
      val a = AlignmentCanonicalParser.extractAlignment(alignment)
      val s = sent.split(" +").toList
      s.foreach(voc(_))
      val chart = AlignmentForestParser.parse(s, a, g)
      allRules ++= this.extractAllRules(chart)
    }
    val expectedCounts = Map[Rule, Double]().withDefaultValue(1.0)
    val newRules:Set[Rule] = allRules.groupBy(_.lhs).flatMap{case (lhs:NonTerm, rules:Set[Rule]) =>
      val lhsExpectedCount = rules.size
      rules.map{
        case rule:InnerRule   => rule.copy(p = expectedCounts(rule)/lhsExpectedCount)
        case rule:PretermRule => rule.copy(p = expectedCounts(rule)/lhsExpectedCount)
      }
    }.toSet

    val newGrammar = new Grammar(
        rulesArg = newRules,
        latentMappings = AlignmentForestParser.defaultLatentMappings,
        nonTerms = AlignmentForestParser.defaultNonTerms,
        latestSplits = List(),
        voc = voc
        )
    
    voc.lock()
    
    newGrammar
  }
  
  private def computeMergeLikelihood(chart:Chart, g:Grammar) : Map[(NonTerm, NonTerm, NonTerm), LogProbability] = {
    val n = chart.size

    val mergeLikelihoodAcc = scala.collection.mutable.Map[(NonTerm, NonTerm, NonTerm), LogProbability]().withDefaultValue(LogNil)

    for(span <- 1 to n){
      for(i <- 0 until n-span+1){
        val j = i + span - 1
        
        val splitLikelihoodLogComplete = logSumExp(chart(i)(j).values.toList.map{ nonTermSpan:NonTermSpan =>
          nonTermSpan.inside + nonTermSpan.outside
        })
        
        g.latestSplitsInts.foreach{ case (mother:NonTerm, split1:NonTerm, split2:NonTerm, p1Raw:Double, p2Raw:Double) =>
          if(chart(i)(j).contains(split1) != chart(i)(j).contains(split2)){
            throw new Exception("span should contain ether both or no splits")
          }
          if(chart(i)(j) contains split1){

            val p1Log = Math.log( p1Raw/(p1Raw + p2Raw) )
            val p2Log = Math.log( p2Raw/(p1Raw + p2Raw) )

            val A1InLog  = chart(i)(j)(split1).inside 
            val A1OutLog = chart(i)(j)(split1).inside 
            
            val A2InLog = chart(i)(j)(split2).inside 
            val A2OutLog = chart(i)(j)(split2).inside 
            
            val AInLog  = logSumExp((p1Log+A1InLog), (p2Log+A2InLog))
            val AOutLog = A1OutLog + A2OutLog
            
            val mergeLikelihoodLogPartial = AInLog+AOutLog
            val splitLikelihoodLogPartial = logSumExp((A1InLog+A1OutLog), (A2InLog+A2OutLog))
            
            // val mergeLikelihoodLogComplete = Math.log(Math.exp(splitLikelihoodLogComplete)-Math.exp(splitLikelihoodLogPartial)+Math.exp(mergeLikelihoodLogPartial))
            val mergeLikelihoodLogComplete = logSumExp(logSubstractExpWithLog1p(splitLikelihoodLogComplete, splitLikelihoodLogPartial), mergeLikelihoodLogPartial)
            
            mergeLikelihoodAcc( (mother, split1, split2) ) += mergeLikelihoodLogComplete - splitLikelihoodLogComplete
          }
        }
        
      }
    }

    mergeLikelihoodAcc.toMap
  }
  
  private def computeExpectedCountPerChart(chart:Chart, nonTerms:IntMapping) : Map[Rule, Double] = {
    val n = chart.size
    val sentProb = Math.exp(chart(0)(n-1)(nonTerms("ROOT")).inside)
    
    val ruleCountAcc = scala.collection.mutable.Map[Rule, Double]().withDefaultValue(0.0)
    
    for(span <- 1 to n){
      for(i <- 0 until n-span+1){
        val j = i + span - 1
        
        for(nonTermSpan <- chart(i)(j).values){
          nonTermSpan.edges.groupBy(_.rule).foreach{ case (rule:Rule, edges:List[Edge]) =>
            ruleCountAcc(rule) += Math.exp(nonTermSpan.outside + logSumExp(edges.map{_.inside}))
          }
        }
      }
    }

    ruleCountAcc.toMap.mapValues{_/sentProb}
  }
  
  private def children(edge:Edge) : List[(Int, Int, NonTerm)] = {
    (childrenSpans(edge) zip edge.rule.asInstanceOf[InnerRule].rhs).map{ case ((x, y), z) => (x,y,z)}
  }
  
  private def childrenSpans(edge:Edge) : List[(Int, Int)] = {
    val allSplits = edge.start :: edge.splits.flatMap(split => List(split-1, split)) ++ List(edge.end)
    allSplits.sliding(2, 2).map{ case List(x, y) => (x, y)}.toList
  }

}