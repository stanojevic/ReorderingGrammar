package grammar.reordering.alignment

import beer.permutation.pet.representation.TreeNode
import beer.permutation.pet.parser.HelperFunctions
import beer.permutation.pet.representation.{Term => TreeTerm}
import beer.permutation.pet.representation.{NonTerm => TreeNonTerm}
import grammar.reordering.representation._
import grammar.reordering.representation.Probability.LogOne

class AlignmentForestParserWithTags (
    g:Grammar,
    attachLeft:Boolean,
    attachRight:Boolean,
    attachTop:Boolean,
    attachBottom:Boolean,
    beSafeBecauseOfPruning:Boolean,
    canonicalOnly:Boolean,
    rightBranching:Boolean
    ){
  
  if(!canonicalOnly && rightBranching){
    throw new IllegalArgumentException(
        s"doesn't make sence to have canonicalOnly=$canonicalOnly && rightBranching=$rightBranching")
  }
  
  def parse(sent:List[String],
      a:Set[(Int, Int)],
      tags:POSseq) : Chart = {
    
    val n = sent.size
    
    val chart = ChartHelper.emptyChart(n)
    
    if(attachLeft && attachTop){
      val tree = AlignmentCanonicalParser.parse(sent.size, a, true, false, rightBranching)
      addToChart(chart, tree, sent, tags)
    }
    if(attachLeft && attachBottom){
      val tree = AlignmentCanonicalParser.parse(sent.size, a, true, true, rightBranching)
      addToChart(chart, tree, sent, tags)
    }
    if(attachRight && attachTop){
      val tree = AlignmentCanonicalParser.parse(sent.size, a, false, false, rightBranching)
      addToChart(chart, tree, sent, tags)
    }
    if(attachRight && attachBottom){
      val tree = AlignmentCanonicalParser.parse(sent.size, a, false, true, rightBranching)
      addToChart(chart, tree, sent, tags)
    }
    
    addStartSymbols(chart)

    chart
  }
  
  private def addToChart(
      chart:Chart,
      tree:TreeNode,
      words:List[String],
      tags:POSseq
      ) : Unit = {
    val collapsedTree = if(canonicalOnly) tree else HelperFunctions.collapseTree(tree)
    
    collapsedTree match {

      case TreeTerm(pos, el) =>
        var edges = List[Edge]()
        val word = g.voc(words(pos))
        for((lhsStr, prob) <- tags(pos)){
          val lhs = g.nonTerms(lhsStr)
          for(latentLhs <- g.latentMappings(lhs)){
            val rule = g.getPretermRule(latentLhs, word, prob)
            edges ::= Edge(pos, pos, rule, List())
          }
        }

        storeEdges(chart, edges)

      case TreeNonTerm(start, end, min, max, op, children) => {
        // recursion
        children.zipWithIndex.foreach{ case (child, index) =>
          addToChart(chart, child, words, tags)
        }

        val lhsStr = opToStr(op)
        val lhs    = g.nonTerms(lhsStr)
        val latentLhss = g.latentMappings(lhs)
        
        val allowedChildNonTerms:List[Traversable[Int]] = children.zipWithIndex.map{ case (child, index) =>
          val childNTstrs:Traversable[String] = nodeToStr(child, tags)
          val childNTs = childNTstrs.map{g.nonTerms(_)}
          val latents = childNTs.flatMap{g.latentMappings(_)}.filter{
            !beSafeBecauseOfPruning || chart(child.left)(child.right).containsKey(_)
          }
          latents
        }

        if(op.size==2){

          for(span <- 2 to children.size){
            val slidingSpansWithAllowedNonTerms = (children zip allowedChildNonTerms).sliding(span)
            for(spannedChildrenWithAllowedNonTerms <- slidingSpansWithAllowedNonTerms){
              val spanStart = spannedChildrenWithAllowedNonTerms.head._1.left
              val spanEnd   = spannedChildrenWithAllowedNonTerms.last._1.right

              val criticalLeftSplit = spannedChildrenWithAllowedNonTerms.head._1.right+1
              val criticalRightSplit = spannedChildrenWithAllowedNonTerms.last._1.left

              val splitPoints = spannedChildrenWithAllowedNonTerms.tail.map{_._1.left}
              for(splitPoint <- splitPoints){
                
                val rhss1:Traversable[NonTerm] = if(splitPoint == criticalLeftSplit){
                  spannedChildrenWithAllowedNonTerms.head._2
                }else{
                  latentLhss.filter { nonTerm => !beSafeBecauseOfPruning || chart(spanStart)(splitPoint-1).containsKey(nonTerm) } // it's the same as rhs in this case
                }
                
                val rhss2:Traversable[NonTerm] = if(splitPoint == criticalRightSplit){
                  spannedChildrenWithAllowedNonTerms.last._2
                }else{
                  latentLhss.filter { nonTerm => !beSafeBecauseOfPruning || chart(splitPoint)(spanEnd).containsKey(nonTerm) } // it's the same as rhs in this case
                }
                
                val allowedChildrenNTs = List(rhss1, rhss2)
                for(lhs <- latentLhss){
                  val newEdges = createEdgesForNonUnaryNode(g, spanStart, spanEnd, List(splitPoint), lhs, allowedChildrenNTs)
                  storeEdges(chart, newEdges)
                }
              }
            }
          }
        }else{
          val motherLhs = lhs
          
          val splitPoints = children.tail.map(_.left)
          
          val allowedChildrenNTs:List[Traversable[NonTerm]] = children.zipWithIndex.map{ case (child, i) =>
            val (left, right) = (child.left, child.right)
            val childNTstrs:Traversable[String] = nodeToStr(child, tags)
            val childNTs = childNTstrs.map{g.nonTerms(_)}
            val allowedChildren = childNTs.flatMap{g.latentMappings(_)}.filter{
              !beSafeBecauseOfPruning || chart(left)(right).containsKey(_)
            }
            allowedChildren
          }
          
          for(latentMotherLhs <- g.latentMappings(motherLhs)){

            val newEdges = createEdgesForNonUnaryNode(g, start, end, splitPoints, latentMotherLhs, allowedChildrenNTs)

            storeEdges(chart, newEdges)
          }
          
        }
      }
    }
  }
  
  private def createEdgesForNonUnaryNode(
      g:Grammar,
      start:Int, end:Int, splitPoints:List[Int],
      latentMotherLhs:NonTerm, allowedChildrenNTs:List[Traversable[NonTerm]]
      ) : List[Edge] = {
    var newNaryEdges = List[Edge]()
    var newUnaryEdges = List[Edge]()
    
    val borders = (start::(splitPoints.flatMap{x => List(x-1, x)})++List(end)).grouped(2).toList
    
    var fakeRhs = List[Int]()
    (borders zip allowedChildrenNTs).zipWithIndex.foreach{
      case ((List(left, right), allowedChildNTs), i) =>
        val latentLhsStr = g.nonTerms(latentMotherLhs)
        val parts = latentLhsStr.split("_")
        
        val fakeUnaryLhs = if(parts.size>1){
          g.nonTerms(parts(0)+"*"+(i+1) + "_"+parts(1))
        }else{
          g.nonTerms(parts(0)+"*"+(i+1) )
        }

        fakeRhs ::= fakeUnaryLhs

        for(child <- allowedChildNTs){
          if(child != fakeUnaryLhs){
            g.getInnerRule(fakeUnaryLhs, List(child)) match {
              case Some(rule) =>
                val newEdge = Edge(left, right, rule, List())
                newUnaryEdges ::= newEdge
              case None =>
            }
          }
        }
    }

    if(!newUnaryEdges.isEmpty){
      fakeRhs = fakeRhs.reverse
      val rule = InnerRule(latentMotherLhs, fakeRhs, LogOne)
      newNaryEdges ::= Edge(start, end, rule, splitPoints)
    }

    newUnaryEdges ++ newNaryEdges
  }
  
  private def storeEdges(chart:Chart, edges : List[Edge]) : Unit = {
    for (edge <- edges) {
      val start = edge.start
      val end = edge.end
      val latentLhs = edge.rule.lhs
      chart(start)(end).putIfAbsent(latentLhs, new NonTermSpan())
      chart(start)(end).get(latentLhs).addEdge(edge)
    }
  }
  
  private def addStartSymbols(chart:Chart) : Unit = {
    val start = 0
    val end = chart.size - 1
    val lhs = g.ROOT
    var edgesToAdd = List[Edge]()
    val it = chart(start)(end).iterator()
    while(it.hasNext()){
      it.advance()
      val nonTermSpan = it.value()
      if(! nonTermSpan.edges.isEmpty){
        val rhs = it.key()
        for(latentLhs <- g.latentMappings(lhs)){
          g.getInnerRule(lhs, List(rhs)) match {
            case Some(rule) => 
              edgesToAdd ::= Edge(start, end, rule, List())
            case None =>
          }
        }
      }
    }
    storeEdges(chart, edgesToAdd)
  }
  
  private def opToStr(op:List[Int]) : String = {
    "P"+op.mkString("")
  }
  
  /**
   * this method is called ONLY in cases of many binary splits
   */
  private def partialNodeListToStr(parentOp:String, nodeList:List[TreeNode], posTags:POSseq) : Traversable[String] = {
    if(nodeList.size>1){
      List(parentOp)
    }else{
      nodeToStr(nodeList.head, posTags:POSseq)
    }
  }
  
  private def nodeToStr(node:TreeNode, posTags:POSseq) : Traversable[String] = {
    node match{
        case TreeNonTerm(start, end, min, max, operator, children) =>
          List(opToStr(operator))
        case TreeTerm(pos, el) => 
          posTags(pos).keys
    }
  }
  
}

object AlignmentForestParserWithTags{

  def createDummyLatentMappings(nonTerms:IntMapping) = {
    nonTerms.allInts.map{ x => x->List(x) }.toMap
  }
    
  def createNonTermsMappingWithTags(tags:List[String]) : IntMapping = {
    
    val arity1nonTerms = Set("A", "N")
    val arity2nonTerms = Set("P01", "P10", "P12", "P21")
    val arity4nonTerms = Set("P2413", "P3142")
    val fakeArity2nonTerms = arity2nonTerms.flatMap{ nt =>
      val size = 2
      (1 to size).map{ i => s"$nt*$i"}
    }
    val fakeArity4nonTerms = arity4nonTerms.flatMap{ nt =>
      val size = 4
      (1 to size).map{ i => s"$nt*$i"}
    }
    val arity5nonTerms = Set("P24153", "P25314", "P42513", "P41352", "P35142", "P31524")
    val fakeArity5nonTerms = arity4nonTerms.flatMap{ nt =>
      val size = 5
      (1 to size).map{ i => s"$nt*$i"}
    }
  
    val outputNonTerm = new IntMapping()
    outputNonTerm(Grammar.ROOTtoken)
    val motherSizesToEncode = List(2) // List(2, 4, 5)
    for(motherSize <- motherSizesToEncode){
      for(mothersChild <- 1 to motherSize){
        for(perm <- arity1nonTerms ++ arity2nonTerms ++ arity4nonTerms ++ arity5nonTerms){
          // outputNonTerm(perm)
          val nt = perm
          outputNonTerm(nt)
          val arity = perm.length-1
          // if(arity>2){
            for(i <- 1 to arity){
              outputNonTerm(s"$nt*$i")
            }
          // }
        }
      }
    }
    
    for(tag <- tags){
      outputNonTerm(tag)
    }
    outputNonTerm("tag_"+Grammar.unknownToken)
    
    outputNonTerm.lock()
    outputNonTerm
  }
  
  
}
