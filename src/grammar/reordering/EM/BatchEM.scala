package grammar.reordering.EM

import grammar.reordering.representation.Probability
import grammar.reordering.representation.Probability.{LogNil}
import grammar.reordering.representation.Grammar
import grammar.reordering.representation.Rule
import grammar.reordering.representation.POSseq
import java.io.File
import java.text.SimpleDateFormat
import java.util.Date
import grammar.reordering.alignment.PhrasePairExtractor
import grammar.reordering.alignment.AlignmentForestParserWithTags
import grammar.reordering.parser.SimpleTreeNode
import java.io.PrintWriter

object BatchEM {

  def runTraining(
                 stoppingCriteria : (Probability, Probability, Int) => Boolean,
                 output : String,
                 trainingData:List[(String, String, POSseq)],
                 initG:Grammar,
                 firstIterNum:Int,
                 threads:Int,
                 threadBatchSize:Int,
                 randomness:Double,
                 extractTreebankInLastIter:Boolean,
                 hardEMtopK:Int,
                 attachLeft:Boolean,
                 attachRight:Boolean,
                 attachTop:Boolean,
                 attachBottom:Boolean,
                 canonicalOnly:Boolean,
                 rightBranching:Boolean,
                 maxRuleProduct:Boolean,
                 maxRuleSum:Boolean
                    ) : Grammar = {
    var initCounts = Map[Rule, Double]()
    for(rule <- initG.allRules){
      initCounts += rule -> 1.0
    }
    var currentCounts = initCounts
    
    var prevLikelihood = LogNil
    var currentLikelihood = LogNil // unimporant initialization
    var it = firstIterNum
    var currentG = initG
    
    val wordCount:Double = trainingData.map{_._1.split(" +").size}.sum

    do{
      val ft = new SimpleDateFormat ("HH:mm dd.MM.yyyy")
      val date = ft.format(new Date())
      System.err.println(s"Iteration $it started at $date")
      System.err.println()

      var extractTreebankNow = false
      val result = if(stoppingCriteria(prevLikelihood, currentLikelihood, it+1)){
        if(hardEMtopK > 0){
          extractTreebankNow = true
          System.err.println("HARD-EM iteration")
        }else{
          extractTreebankNow = false
          System.err.println("SOFT-EM iteration")
        }
        iteration(trainingData, currentG, threadBatchSize, threads, randomness, hardEMtopK, attachLeft, attachRight, attachTop, attachBottom, canonicalOnly, rightBranching, extractTreebankNow, maxRuleProduct, maxRuleSum)
      }else{
        System.err.println("SOFT-EM iteration")
        iteration(trainingData, currentG, threadBatchSize, threads, randomness, -1, attachLeft, attachRight, attachTop, attachBottom, canonicalOnly, rightBranching, extractTreebankNow, maxRuleProduct, maxRuleSum)
      }
      currentG = result._1
      val treebank = result._2
      if(extractTreebankNow){
        saveTreebank(output+"/treebank_"+it+".temporary", (treebank zip trainingData.map{_._1}), dephrased=false)
        saveTreebank(output+"/treebank_"+it+".final", (treebank zip trainingData.map{_._1}), dephrased=true)
      }
      currentLikelihood = result._3
      
      currentG.save(output+"/grammar_"+it+".temporary", dephrased=false)
      // val dePhrasedGrammar = PhrasePairExtractor.unfoldGrammarOfIdioms(currentG)
      // dePhrasedGrammar.save(output+"/grammar_"+it+".dephrased")
      currentG.save(output+"/grammar_"+it+".final", dephrased=true)
      val perplexityPerWord = Math.exp(-currentLikelihood.log/wordCount)
      System.err.println()
      if(maxRuleSum && extractTreebankNow){
        System.err.println(s"Grammar $it: likelihood $currentLikelihood DON'T TRUST THIS NUMBER MUCH")
      }else{
        System.err.println(s"Grammar $it: likelihood $currentLikelihood")
      }
      System.err.println(s"Grammar $it: Perplexity per word $perplexityPerWord")
      System.err.println()
      
      it += 1
    }while( ! stoppingCriteria(prevLikelihood, currentLikelihood, it))
      
    currentG
  }
  
  private def saveTreebank(fnOut:String, treebank : List[(List[SimpleTreeNode], String)], dephrased:Boolean) : Unit = {
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
    
    treebank.zipWithIndex.foreach{ case ((trees, sent), index) =>
      val words = sent.split(" ").toList
      trees.zipWithIndex.foreach{ case (tree, rank) =>
        pw.print(s"sent=$index rank=$rank ")
        pw.print(tree.toPennString(words, printDephrased=dephrased))
        pw.println()
      }
    }
    
    pw.close()
  }
  
  private def iteration(
                 trainingData:List[(String, String, POSseq)],
                 g:Grammar,
                 batchSize:Int,
                 threads:Int,
                 randomness:Double,
                 hardEMtopK:Int,
                 attachLeft:Boolean,
                 attachRight:Boolean,
                 attachTop:Boolean,
                 attachBottom:Boolean,
                 canonicalOnly:Boolean,
                 rightBranching:Boolean,
                 extractTreebank:Boolean,
                 maxRuleProduct:Boolean,
                 maxRuleSum:Boolean
                    ) : (Grammar, List[List[SimpleTreeNode]], Probability) = {
    System.err.println(s"STARTED expectations")
    val t1 = System.currentTimeMillis()
    
    val (expectedCounts, treebank, likelihood) = InsideOutside.expectation(trainingData, g, batchSize, threads, randomness, hardEMtopK, attachLeft, attachRight, attachTop, attachBottom, canonicalOnly, rightBranching, extractTreebank, maxRuleProduct, maxRuleSum)
    
    val t2 = System.currentTimeMillis()
    val period = t2 - t1
    System.err.println(s"DONE expectations took $period ms")

    val newGrammar = InsideOutside.maximization(g, expectedCounts)
    (newGrammar, treebank, likelihood)
  }

}
