package grammar.reordering.EM

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import grammar.reordering.representation.IntMapping
import grammar.reordering.representation.Rule
import grammar.reordering.representation.NonTerm
import grammar.reordering.representation.Grammar
import grammar.reordering.representation.`package`.InnerRule
import grammar.reordering.representation.`package`.PretermRule
import grammar.reordering.representation.Probability
import grammar.reordering.representation.Probability.{LogNil, LogOne}
import java.io.PrintWriter
import scala.io.Source

class SplitMergeTest extends FlatSpec with ShouldMatchers{

  "splitting" should "not break" in {
    val alignment = "1-1 2-0 3-0 4-0"
    val sent = "Nekakva recenica koja nema mnogo smisla"
    
    val g = InsideOutside.initialIteration(List(sent), List(alignment))
    val g1 = SplitMerge.split(g, Map().withDefaultValue(1.0))
    val g2 = SplitMerge.split(g1, Map().withDefaultValue(1.0))

    // println("\nGrammar 0\n"+g)
    // println("\nGrammar 1\n"+g1)
    // println("\nGrammar 2\n"+g2)
  }

  "splitting and training" should "not break" in {
    val sents = List(
        "Nekakva recenica koja nema mnogo smisla"//,
        //"Nekakva recenica koja nema mnogo smisla",
        //"Nekakva recenica koja nema mnogo smisla",
        //"Nekakva recenica koja nema mnogo smisla"
        )
    val alignments = List(
        "1-1 2-0 3-0 4-0"//,
        //"1-1 2-0 3-1 4-0",
        //"1-1 2-0 3-2 4-0",
        //"1-3 2-0 3-3 4-1"
        )

    val batchSize = 1
    val parallel = true
    
    val gInit = InsideOutside.initialIteration(sents, alignments)
    val gSplitInit = SplitMerge.split(gInit, Map().withDefaultValue(1.0))
    val gSplitInit2 = SplitMerge.split(gSplitInit, Map().withDefaultValue(1.0))
    val gSplitInit3 = SplitMerge.split(gSplitInit2, Map().withDefaultValue(1.0))
    val gSplitInit4 = SplitMerge.split(gSplitInit3, Map().withDefaultValue(1.0))
    
    var grammars = List[Grammar](gSplitInit4)
    var likelihoods = List[Probability](LogNil)
    var mergeLikelihoods = List[Map[(NonTerm, NonTerm, NonTerm), Probability]]()
    
    var iteration = 1
    var difference = LogOne
    
    // val limit = 0.000000000000000000000000001
    val limit = Double.NegativeInfinity 
    
    while(difference.toDouble>=limit && iteration < 30){
      println(s"Starting training iteration $iteration")
      val (expectedCounts, mergeLikelihood, likelihood) = InsideOutside.expectation(sents, alignments, grammars.head, batchSize, parallel)
      val gNew = InsideOutside.maximization(grammars.head, expectedCounts)
      
      difference = likelihood - likelihoods.head
      
      if(difference.toDouble>=limit){
        grammars ::= gNew
        likelihoods ::= likelihood
        mergeLikelihoods ::= mergeLikelihood
      }else{
        println("bad stuff")
      }
      println(s"\nGrammar $iteration $likelihood\n")
      println(s"\t\t\t\tdifference $difference")
      iteration += 1
    }
    println(s"\nBest grammar "+grammars.size+" "+likelihoods.head+"\n")
    SplitMerge.smoothSplits(grammars.head).save("./grammars/grammar.txt")

    // val streamMerge = new PrintWriter("./mergeLikelihood.txt")
    // for(el <- mergeLikelihoods.last){
    //   streamMerge.println(el)
    // }
    // if(streamMerge != System.out)
    //   streamMerge.close()
  }

  "loading and resaving the grammar" should "not break" in {
    val g = Grammar.loadFromFile("./grammars/grammar3.txt")
    g.save("./grammars/grammar2.txt")
  }
  
  "full training" should "be fast" in {
    val rawRawSents = Source.fromFile("data/top1000_english").getLines.toList
    val rawSents = rawRawSents.map{_.split(" +").toList}
    val rawAlignments = Source.fromFile("data/top1000_alignments").getLines.toList
    val alignments = rawAlignments.map{AlignmentCanonicalParser.extractAlignment(_)}
    
    val maxUnknownCount = 3
    val sents = Preprocessing.prepareTrainingDataForUnknownWords(rawSents, maxUnknownCount)
    
    val filtered = (sents zip alignments).filter{ case (sent, a) =>
      val arity = Preprocessing.maxArity(a)
      arity <= 5 &&
      Preprocessing.numAlignedWords(a) >=2
    }
    println("Selected: "+filtered.size)
    
    val t1 = System.currentTimeMillis()
    var trainingSents = filtered.map{_._1.mkString(" ")}
    var trainingAlignments = filtered.map{_._2.map{case (i, j) => s"$i-$j"}.mkString(" ")}
    val initG = InsideOutside.initialIteration(trainingSents, trainingAlignments)
    val t2 = System.currentTimeMillis()
    val periodInit = t2-t1
    println(s"time for init run: $periodInit ms")
    
    val t3 = System.currentTimeMillis()
    initG.save("grammars/big_grammar")
    val t4 = System.currentTimeMillis()
    val periodSaving = t4-t3
    println(s"time for saving run: $periodSaving ms")
    
    var currentG = initG
    
    val splitIts = 3
    for(splitIt <- 1 to splitIts){
      println(s"SPLIT $splitIt")
      currentG = SplitMerge.split(currentG, Map().withDefaultValue(1.0))
      println(s"DONE SPLIT $splitIt")
    }
    
    

    trainingSents = trainingSents.take(10)
    trainingAlignments = trainingAlignments.take(10)
    val iterations = 9
    val batchSize = 5
    val parallel = false
    for(iter <- 1 to iterations){
      println("STARTING STUPID ITERATION")
      val t5 = System.currentTimeMillis()
      val res = InsideOutside.iteration(trainingSents, trainingAlignments, currentG, batchSize, parallel)
      currentG = res._1 
      val likelihood = res._2
      val t6 = System.currentTimeMillis()
      val iterPeriod = t6-t5
      println(s"iteration $iter took $iterPeriod ms    likelihood $likelihood or "+Math.exp(likelihood))
      val t7 = System.currentTimeMillis()
      currentG.save(s"grammars/grammar_iter_$iter")
      val t8 = System.currentTimeMillis()
      val savingPeriod = t8-t7
      println(s"iteration $iter saving took $savingPeriod ms")
    }
    
  }

}