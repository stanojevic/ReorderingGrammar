package grammar.reordering.EM

import grammar.reordering.representation._
import grammar.reordering.representation.Probability.{LogNil, LogOne, sum, product}
import java.io.PrintWriter

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
  
  def inside(chart:Chart, g:Grammar) : Unit = {
    val n = chart.size
    
    for(i <- 0 until n){
      chart(i)(i).values.foreach{ nonTermSpan =>
        nonTermSpan.inside = sum(nonTermSpan.edges.map{_.rule.prob})
        nonTermSpan.edges.foreach{ edge =>
          edge.inside = edge.rule.prob
        }
      }
    }
    
    for(span <- 2 to n){
      for(i <- 0 until n-span+1){
        val j = i + span - 1
        
        val allEdges:List[Edge] = chart(i)(j).values.flatMap(_.edges).toList
        
        val (unaryEdges, naryEdges) = allEdges.partition(_.rule.asInstanceOf[InnerRule].rhs.size == 1)

        for(processingUnary <- List(false, true)){
          val edgesToProcess = if(processingUnary) unaryEdges else naryEdges
          for(edge <- edgesToProcess){
            edge.inside = edge.rule.prob * 
                             product(edge.children.map{ case (start:Int, end:Int, childNonTerm:NonTerm) =>
                               chart(start)(end)(childNonTerm).inside
                             })
          }
  
          for(nonTermSpan <- chart(i)(j).values){
            nonTermSpan.inside = sum(nonTermSpan.edges.map{_.inside})
          }
        }
      }
    }
  }
  
  def outside(chart:Chart, g:Grammar) : Unit = {
    val n = chart.size
    
    for(i <- 0 until n){
      for(j <- 0 until n){
        chart(i)(j).values.foreach(_.outside = LogNil)
      }
    }
    chart(0)(n-1)(g.ROOT).outside = LogOne
    
    for(span <- n to 2 by -1){
      for(i <- 0 until n-span+1){
        val j = i + span - 1
        
        val allEdgesWithNTS = chart(i)(j).values.flatMap(nonTermSpan => nonTermSpan.edges.map{(_, nonTermSpan)})
        
        val (unaryEdgesWithNTS, naryEdgesWithNTS) = allEdgesWithNTS.partition(_._1.rule.asInstanceOf[InnerRule].rhs.size == 1)

        for(processingUnary <- List(true, false)){
          val edgesWithNTS = if(processingUnary) unaryEdgesWithNTS else naryEdgesWithNTS
          for((edge, nonTermSpan) <- edgesWithNTS){
            for((start, end, childNonTerm) <- edge.children){
              chart(start)(end)(childNonTerm).outside += nonTermSpan.outside * edge.inside / chart(start)(end)(childNonTerm).inside
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
    (newGrammar, allSplitLikelihood.log)
  }
  
  def expectation( sents:List[String],
                 alignments:List[String],
                 g:Grammar,
                 batchSize:Int,
                 parallel:Boolean
                    ) : (Map[Rule, Double], Map[(NonTerm, NonTerm, NonTerm), Probability], Probability) = {
    g.voc.lock()
    val trainingBatches = if(parallel)
                           (sents zip alignments).grouped(batchSize).toList.par
                         else
                           (sents zip alignments).grouped(batchSize).toList
    var processed = 0

    val (
        expectedCounts:Map[Rule, Double],
        mergeLikelihood:Map[(NonTerm, NonTerm, NonTerm), Probability],
        allSplitLikelihood:Probability) = trainingBatches.map{ miniBatch:List[(String, String)] =>
      miniBatch.map{ case (sent, alignment) =>

        val a = AlignmentCanonicalParser.extractAlignment(alignment)
        val s = sent.split(" +").toList
  
        var t1 = System.currentTimeMillis()
        val chart = AlignmentForestParser.parse(s, a, g)
        var t2 = System.currentTimeMillis()
        println((t2-t1)+" ms for forest parsing")

        t1 = System.currentTimeMillis()
        this.inside(chart, g)
        t2 = System.currentTimeMillis()
        println((t2-t1)+" ms for inside")

        t1 = System.currentTimeMillis()
        this.outside(chart, g)
        t2 = System.currentTimeMillis()
        println((t2-t1)+" ms for outside")

        t1 = System.currentTimeMillis()
        val chartExpectations = this.computeExpectedCountPerChart(chart, g)
        t2 = System.currentTimeMillis()
        println((t2-t1)+" ms for computeExpectedCountPerChart")

        t1 = System.currentTimeMillis()
        val mergeLikelihood = this.computeMergeLikelihood(chart, g)
        t2 = System.currentTimeMillis()
        println((t2-t1)+" ms for computeMergeLikelihood")

        val n = chart.size
        val sentProb = chart(0)(n-1)(g.ROOT).inside

        processed += 1
        if(processed % 10 == 0){
          println(processed)
          t1 = System.currentTimeMillis()
          System.gc()
          t2 = System.currentTimeMillis()
          println((t2-t1)+" ms for gc")
        }

        (chartExpectations, mergeLikelihood, sentProb)
      }.reduce( sumExpectations )
    }.reduce( sumExpectations )
    
    (expectedCounts.withDefaultValue(0.0), mergeLikelihood, allSplitLikelihood)
  }
  
  def maximization(g:Grammar, expectedCounts:Map[Rule, Double]) : Grammar = {
    val newRules:Set[Rule] = g.allRules.groupBy(_.lhs).flatMap{case (lhs:NonTerm, rules:Set[Rule]) =>
      val lhsExpectedCount:Double = rules.toList.map{ rule =>
        expectedCounts(rule)
      }.sum
      if(lhsExpectedCount == 0.0){
        List()
      }else{
        rules.map{
          case rule:InnerRule   => rule.copy(p = Probability(expectedCounts(rule)/lhsExpectedCount))
          case rule:PretermRule => rule.copy(p = Probability(expectedCounts(rule)/lhsExpectedCount))
        }
      }
    }.toSet
    
    val newGrammar = new Grammar(rulesArg = newRules, latentMappings = g.latentMappings, voc = g.voc, nonTerms = g.nonTerms, latestSplits = g.latestSplits )
    
    newGrammar
  }
  
  private def sumExpectations(
      a:(Map[Rule, Double], Map[(NonTerm, NonTerm, NonTerm), Probability], Probability),
      b:(Map[Rule, Double], Map[(NonTerm, NonTerm, NonTerm), Probability], Probability))
      : (Map[Rule, Double], Map[(NonTerm, NonTerm, NonTerm), Probability], Probability) = { 
    val (aExp, aMerge, aLikelihood) = a
    val (bExp, bMerge, bLikelihood) = b
    val cExp = (aExp.keySet ++ bExp.keySet).map{key => key -> (aExp.getOrElse(key, 0.0) + bExp.getOrElse(key, 0.0))}.toMap
    val cMerge = (aMerge.keySet ++ bMerge.keySet).map{key => key -> (aMerge.getOrElse(key, LogNil) * bMerge.getOrElse(key, LogNil))}.toMap
    val cLikelihood = aLikelihood * bLikelihood
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
      //Grammar.testNonRepeatingRules(rules)
      rules.map{
        case rule:InnerRule   => rule.copy(p = Probability(expectedCounts(rule)/lhsExpectedCount))
        case rule:PretermRule => rule.copy(p = Probability(expectedCounts(rule)/lhsExpectedCount))
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
  
  private def computeMergeLikelihood(chart:Chart, g:Grammar) : Map[(NonTerm, NonTerm, NonTerm), Probability] = {
    val n = chart.size

    val mergeLikelihoodAcc = scala.collection.mutable.Map[(NonTerm, NonTerm, NonTerm), Probability]().withDefaultValue(LogNil)

    for(span <- 1 to n){
      for(i <- 0 until n-span+1){
        val j = i + span - 1
        
        val splitLikelihoodComplete = chart(i)(j).values.toList.map{ nonTermSpan:NonTermSpan =>
          (nonTermSpan.inside * nonTermSpan.outside).toDouble
        }.sum
        
        g.latestSplitsInts.foreach{ case (mother:NonTerm, split1:NonTerm, split2:NonTerm, p1Raw:Double, p2Raw:Double) =>
          if(chart(i)(j).contains(split1) != chart(i)(j).contains(split2)){
            throw new Exception("span should contain ether both or no splits")
          }
          if(chart(i)(j) contains split1){

            val p1 = Probability( p1Raw/(p1Raw + p2Raw) )
            val p2 = Probability( p2Raw/(p1Raw + p2Raw) )

            val A1In  = chart(i)(j)(split1).inside 
            val A1Out = chart(i)(j)(split1).outside
            
            val A2In = chart(i)(j)(split2).inside 
            val A2Out = chart(i)(j)(split2).outside
            
            val AIn  = p1*A1In + p2*A2In
            val AOut = A1Out + A2Out
            
            val mergeLikelihoodPartial:Probability = AIn * AOut
            val splitLikelihoodPartial:Probability = A1In*A1Out + A2In*A2Out
            
            val mergeLikelihoodComplete = splitLikelihoodComplete - (splitLikelihoodPartial.toDouble - mergeLikelihoodPartial.toDouble)

            val difference = if(mergeLikelihoodComplete > splitLikelihoodComplete){
              LogOne // something is wrong
            }else{
              // Probability(mergeLikelihoodComplete / splitLikelihoodComplete)
              LogOne // THIS IS WRONG FOR SURE
            }

            mergeLikelihoodAcc( (mother, split1, split2) ) *= difference
          }
        }
        
      }
    }

    mergeLikelihoodAcc.toMap
  }
  
  private def computeExpectedCountPerChart(chart:Chart, g:Grammar) : Map[Rule, Double] = {
    val n = chart.size
    val sentProb = chart(0)(n-1)(g.ROOT).inside
    
    val ruleCountAcc = scala.collection.mutable.Map[Rule, Probability]().withDefaultValue(LogNil)
    
    for(span <- 1 to n){
      for(i <- 0 until n-span+1){
        val j = i + span - 1
        
        for(nonTermSpan <- chart(i)(j).values){
          for(edge <- nonTermSpan.edges){
            ruleCountAcc(edge.rule) += nonTermSpan.outside * edge.inside
          }
        }
      }
    }

    ruleCountAcc.toMap.mapValues{count:Probability =>
      count.toDouble/sentProb.toDouble
    }
  }
  
}
