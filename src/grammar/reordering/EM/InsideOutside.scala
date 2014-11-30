package grammar.reordering.EM

import grammar.reordering.representation._
import grammar.reordering.representation.Probability.{LogNil, LogOne, sum, product}
import java.io.PrintWriter
import scala.collection.parallel.ForkJoinTaskSupport

object InsideOutside {
  
  def extractRuleCounts(chart:Chart) : Map[Rule, Int] = {
    val n = chart.size
    var ruleCounts = scala.collection.mutable.Map[Rule, Int]().withDefaultValue(0)
    for(span <- 1 to n){
      for(i <- 0 until n-span+1){
        val j = i + span - 1
        
        val it = chart(i)(j).iterator()
        while(it.hasNext()){
          it.advance()
          for(edge <- it.value().edges){
            ruleCounts(edge.rule) += 1
          }
        }
        
      }
    }
    
    ruleCounts.toMap
  }
  
  def inside(chart:Chart, g:Grammar) : Unit = {
    val n = chart.size
    
    for(i <- 0 until n){
      val it = chart(i)(i).iterator()
      
      while(it.hasNext()){
        it.advance()
        val nonTermSpan = it.value()
        nonTermSpan.inside = sum(nonTermSpan.edges.map{_.rule.prob}.toList)
        nonTermSpan.edges.foreach{ edge =>
          edge.inside = edge.rule.prob
        }
      }
      
    }
    
    for(span <- 2 to n){
      for(i <- 0 until n-span+1){
        val j = i + span - 1
        
        var allEdges:List[Edge] = List()
        val edgesIt = chart(i)(j).iterator()
        while(edgesIt.hasNext()){
          edgesIt.advance()
          allEdges ++= edgesIt.value().edges
        }
        
        val (unaryEdges, naryEdges) = allEdges.partition(_.rule.asInstanceOf[InnerRule].rhs.size == 1)

        for(processingUnary <- List(false, true)){
          val edgesToProcess = if(processingUnary) unaryEdges else naryEdges
          for(edge <- edgesToProcess){
            edge.inside = edge.rule.prob * 
                             product(edge.children.map{ case (start:Int, end:Int, childNonTerm:NonTerm) =>
                               chart(start)(end).get(childNonTerm).inside
                             })
          }
  
          val nonTermSpanIt = chart(i)(j).iterator()
          while(nonTermSpanIt.hasNext()){
            nonTermSpanIt.advance()
            val nonTermSpan = nonTermSpanIt.value()
            nonTermSpan.inside =  sum(nonTermSpan.edges.map{_.inside}.toList)
          }
        }
      }
    }
  }
  
  def outside(chart:Chart, g:Grammar) : Unit = {
    val n = chart.size
    
    for(i <- 0 until n){
      for(j <- 0 until n){
        val it = chart(i)(j).iterator()
        while(it.hasNext()){
          it.advance()
          it.value().outside = LogNil
        }
      }
    }
    chart(0)(n-1).get(g.ROOT).outside = LogOne
    
    for(span <- n to 2 by -1){
      for(i <- 0 until n-span+1){
        val j = i + span - 1
        
        var unaryEdgesWithNTS = List[(Edge, NonTermSpan)]()
        var naryEdgesWithNTS  = List[(Edge, NonTermSpan)]()
        val it = chart(i)(j).iterator()
        while(it.hasNext()){
          it.advance()
          val nonTermSpan =  it.value()
          val edges = nonTermSpan.edges
          for(edge <- edges){
            if(edge.rule.asInstanceOf[InnerRule].rhs.size == 1){
              unaryEdgesWithNTS ::= (edge, nonTermSpan)
            }else{
              naryEdgesWithNTS ::= (edge, nonTermSpan)
            }
          }
        }

        for(processingUnary <- List(true, false)){
          val edgesWithNTS = if(processingUnary) unaryEdgesWithNTS else naryEdgesWithNTS
          for((edge, nonTermSpan) <- edgesWithNTS){
            for((start, end, childNonTerm) <- edge.children){
              chart(start)(end).get(childNonTerm).outside += nonTermSpan.outside * edge.inside / chart(start)(end).get(childNonTerm).inside
            }
          }
        }
      }
    }
  }
  
  def iteration( trainingData:List[(String, String)],
                 g:Grammar,
                 batchSize:Int,
                 threads:Int
                    ) : (Grammar, Double) = {
    val (expectedCounts, likelihood) = expectation(trainingData, g, batchSize, threads)
    val newGrammar = maximization(g, expectedCounts)
    (newGrammar, likelihood.log)
  }
  
  def expectation(
                 trainingData:List[(String, String)],
                 g:Grammar,
                 batchSize:Int,
                 threads:Int
                    ) : (Map[Rule, Double], Probability) = {
    g.voc.lock()
    val trainingBatches = if(threads>1){
                            val l = trainingData.grouped(batchSize).toList.par
                            l.tasksupport = 
                              new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(threads))
                            l
                          }else{
                            trainingData.grouped(batchSize).toList
                          }
    var processed = 0

    val (
        expectedCounts:Map[Rule, Double],
        allSplitLikelihood:Probability) = trainingBatches.map{ miniBatch:List[(String, String)] =>
      miniBatch.map{ case (sent, alignment) =>

        val a = AlignmentCanonicalParser.extractAlignment(alignment)
        val s = sent.split(" +").toList
  
        // var t1 = System.currentTimeMillis()
        val chart:Chart = AlignmentForestParser.parse(s, a, g)
        // var t2 = System.currentTimeMillis()
        // println((t2-t1)+" ms for forest parsing")

        // t1 = System.currentTimeMillis()
        this.inside(chart, g)
        // t2 = System.currentTimeMillis()
        // println((t2-t1)+" ms for inside")

        // t1 = System.currentTimeMillis()
        this.outside(chart, g)
        // t2 = System.currentTimeMillis()
        // println((t2-t1)+" ms for outside")

        // t1 = System.currentTimeMillis()
        val chartExpectations:Map[Rule, Double] = this.computeExpectedCountPerChart(chart, g)
        // t2 = System.currentTimeMillis()
        // println((t2-t1)+" ms for computeExpectedCountPerChart")

        val n = chart.size
        val sentProb:Probability = chart(0)(n-1).get(g.ROOT).inside

        processed += 1
        if(processed % 10 == 0){
          System.err.println(processed)
          // t1 = System.currentTimeMillis()
          // System.gc()
          // t2 = System.currentTimeMillis()
          // println((t2-t1)+" ms for gc")
        }

        (chartExpectations, sentProb)
      }.reduce( (a,b) => sumExpectations(a, b) )
    }.reduce( (a,b) => sumExpectations(a,b) )
    
    (expectedCounts.withDefaultValue(0.0), allSplitLikelihood)
  }
  
  def maximization(g:Grammar, expectedCounts:scala.collection.Map[Rule, Double]) : Grammar = {
    val newRules:Set[Rule] = g.allRules.groupBy{ rule:Rule => rule.lhs }.flatMap{case (lhs:NonTerm, rules:Stream[Rule]) =>
      val lhsExpectedCount:Double = rules.toList.map{ rule =>
        expectedCounts(rule)
      }.sum
      if(lhsExpectedCount == 0.0){
        List()
      }else{
        rules.foreach{rule =>
          if((expectedCounts(rule)/lhsExpectedCount).isNaN){
            val r1 = rules.toList
            val r2 = rules.toList
            val nums:List[Double] = r1.map{expectedCounts.getOrElse(_, 0.0)}
            println("nan")
            r1++r2
          }
        }
        rules.map{
          case rule:InnerRule   => rule.copy(p = Probability(expectedCounts(rule)/lhsExpectedCount))
          case rule:PretermRule => rule.copy(p = Probability(expectedCounts(rule)/lhsExpectedCount))
        }
      }
    }.toSet
    
    val newGrammar = new Grammar(rulesArg = newRules, latentMappings = g.latentMappings, voc = g.voc, nonTerms = g.nonTerms )
    
    newGrammar
  }
  
  private def sumExpectations(
      a:(Map[Rule, Double], Probability),
      b:(Map[Rule, Double], Probability))
      : (Map[Rule, Double], Probability) = { 
    val (aExp, aLikelihood) = a
    val (bExp, bLikelihood) = b
    val cExp = scala.collection.mutable.Map[Rule, Double]()
    for((k, v) <- aExp){
      cExp(k) = v
    }
    for((k, v) <- bExp){
      cExp(k) = cExp.getOrElse(k, 0.0) + v
    }
    // val cExp = (aExp.keySet ++ bExp.keySet).map{key => key -> (aExp.getOrElse(key, 0.0) + bExp.getOrElse(key, 0.0))}.toMap
    val cLikelihood = aLikelihood * bLikelihood
    (cExp.toMap, cLikelihood)
  }
  
  /**
   * MUST NOT be parallelized because it contains mutable vocabulary (and maybe some other things)
   * anyway this phase is really fast so again NO NEED for parallelization
   */
  def initialIteration( trainingData:List[(String, String)]) : Grammar = {
    val voc = new IntMapping()
    var allRuleCounts = scala.collection.mutable.Map[Rule, Double]().withDefaultValue(0.0)
    val g = new Grammar(
        rulesArg = Set(),
        latentMappings = AlignmentForestParser.defaultLatentMappings,
        nonTerms = AlignmentForestParser.defaultNonTerms,
        voc = voc,
        dummy=true
        )
    var processed = 0
    trainingData.foreach{ case (sent, alignment) =>
      val a = AlignmentCanonicalParser.extractAlignment(alignment)
      val s = sent.split(" +").toList
      s.foreach(voc(_))
      val chart:Chart = AlignmentForestParser.parse(s, a, g)
      for((rule, count) <- this.extractRuleCounts(chart)){
        allRuleCounts(rule) = allRuleCounts(rule)+count
      }
      processed += 1
      if(processed % 100 == 0){
        System.err.println(processed)
      }
    }
    val newRules:Set[Rule] = allRuleCounts.keySet.groupBy{rule:Rule => rule.lhs}.flatMap{case (lhs:NonTerm, rules:Set[Rule]) =>
      val lhsExpectedCount:Double = rules.toList.map{allRuleCounts(_)}.sum
      //Grammar.testNonRepeatingRules(rules)
      rules.map{
        case rule:InnerRule   => rule.copy(p = Probability(allRuleCounts(rule) / lhsExpectedCount))
        case rule:PretermRule => rule.copy(p = Probability(allRuleCounts(rule) / lhsExpectedCount))
      }
    }.toSet.asInstanceOf[Set[Rule]]

    val newGrammar = new Grammar(
        rulesArg = newRules,
        latentMappings = AlignmentForestParser.defaultLatentMappings,
        nonTerms = AlignmentForestParser.defaultNonTerms,
        voc = voc
        )
    
    voc.lock()
    
    newGrammar
  }
  
  def computeExpectedCountPerChart(chart:Chart, g:Grammar) : Map[Rule, Double] = {
    val n = chart.size
    val sentProb = chart(0)(n-1).get(g.ROOT).inside
    
    val ruleCountAcc = scala.collection.mutable.Map[Rule, Probability]().withDefaultValue(LogNil)
    
    for(span <- 1 to n){
      for(i <- 0 until n-span+1){
        val j = i + span - 1
        
        val it = chart(i)(j).iterator()
        while(it.hasNext()){
          it.advance()
          val nonTermSpan = it.value()
          for(edge <- nonTermSpan.edges){
            ruleCountAcc(edge.rule) += nonTermSpan.outside * edge.inside
          }
        }
      }
    }

    ruleCountAcc.toMap.mapValues{count:Probability =>
      count.toDouble/sentProb.toDouble
    }.withDefaultValue(0.0)
  }
  
}
