package grammar.reordering.EM

import grammar.reordering.representation._
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
      chart(i)(i).values.foreach(nonTermSpan => nonTermSpan.inside = logSumExp(nonTermSpan.edges.toList.map{_.rule.logProb}))
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
          chart(i)(j)(lhs).inside = logSumExp(chart(i)(j)(lhs).edges.toList.map{_.inside})
        }
      }
    }
  }
  
  def outside(chart:Chart, nonTerms:IntMapping) : Unit = {
    val n = chart.size
    
    for(i <- 0 until n){
      for(j <- 0 until n){
        chart(i)(j).values.foreach(_.outside = logNil)
      }
    }
    chart(0)(n-1)(nonTerms("ROOT")).outside = logOne
    
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
                    ) : Grammar = {
    g.voc.lock()
    val trainingBatches = if(parallel)
                           (sents zip alignments).grouped(batchSize).toList.par
                         else
                           (sents zip alignments).grouped(batchSize).toList
    val expectedCounts:Map[Rule, Double] = trainingBatches.map{ miniBatch:List[(String, String)] =>
      miniBatch.map{ case (sent, alignment) =>
        val a = AlignmentCanonicalParser.extractAlignment(alignment)
        val s = sent.split(" +").toList
        s.foreach(g.voc(_)) // just checking whether every word is in vocabulary
  
        val chart = AlignmentForestParser.parse(s, a, g)
        this.inside(chart, g.nonTerms)
        this.outside(chart, g.nonTerms)
        val chartExpectations = this.computeExpectedCount(chart, g.nonTerms)
        chartExpectations
      }.reduce((a:Map[Rule, Double],b:Map[Rule, Double]) => 
        (a.keySet ++ b.keySet).map{key => key -> (a.getOrElse(key, 0.0) + b.getOrElse(key, 0.0))}.toMap
      )
    }.reduce((a:Map[Rule, Double],b:Map[Rule, Double]) => 
      (a.keySet ++ b.keySet).map{key => key -> (a.getOrElse(key, 0.0) + b.getOrElse(key, 0.0))}.toMap
    )

    val newRules:Set[Rule] = g.allRules.groupBy(_.lhs).flatMap{case (lhs:NonTerm, rules:Set[Rule]) =>
      val lhsExpectedCount:Double = rules.toList.map{expectedCounts(_)}.sum
      rules.map{
        case rule:InnerRule   => rule.copy(p = expectedCounts(rule)/lhsExpectedCount)
        case rule:PretermRule => rule.copy(p = expectedCounts(rule)/lhsExpectedCount)
      }
    }.toSet
    
    val newGrammar = new Grammar(rulesArg = newRules, latentMappings = g.latentMappings, voc = g.voc, nonTerms = g.nonTerms)
    
    newGrammar
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
        voc = voc
        )
    
    voc.lock()
    
    newGrammar
  }
  
  def computeExpectedCount(chart:Chart, nonTerms:IntMapping) : Map[Rule, Double] = {
    val n = chart.size
    val sentProb = Math.exp(chart(0)(n-1)(nonTerms("ROOT")).inside)
    
    val ruleCountAcc = scala.collection.mutable.Map[Rule, Double]().withDefaultValue(0.0)
    
    for(span <- 1 to n){
      for(i <- 0 until n-span+1){
        val j = i + span - 1
        
        for(nonTermSpan <- chart(i)(j).values){
          nonTermSpan.edges.toList.groupBy(_.rule).foreach{ case (rule:Rule, edges:List[Edge]) =>
            ruleCountAcc(rule) += Math.exp(nonTermSpan.outside + logSumExp(edges.toList.map{_.inside}))
          }
        }
        
        ruleCountAcc
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