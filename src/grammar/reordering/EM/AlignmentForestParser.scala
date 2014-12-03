package grammar.reordering.EM

import beer.permutation.pet.representation.TreeNode
import beer.permutation.pet.parser.HelperFunctions
import beer.permutation.pet.representation.Term
import beer.permutation.pet.representation.NonTerm
import grammar.reordering.representation._

object AlignmentForestParser {
  
  val defaultNonTermsUnmarked = createNonTermsMapping(false)
  val defaultLatentMappingsUnmarked = defaultNonTermsUnmarked.allInts.map{ x => x->List(x)}.toMap
    
  val defaultNonTermsMarked = createNonTermsMapping(true)
  val defaultLatentMappingsMarked = defaultNonTermsMarked.allInts.map{ x => x->List(x)}.toMap
    
  def createNonTermsMapping(markChildOrder:Boolean) : IntMapping = {
    val outputNonTerm = new IntMapping()
    outputNonTerm(Grammar.ROOTtoken)
    for(motherSize <- List(2, 4, 5)){
      for(mothersChild <- 1 to motherSize){
        for(perm <- List("A", "N", "P01", "P10", "P12", "P21", "P2413", "P3142", "P24153", "P25314", "P42513", "P41352", "P35142", "P31524")){
          outputNonTerm(perm)
          val motherSizeDesc   = if(markChildOrder) motherSize   else 0
          val mothersChildDesc = if(markChildOrder) mothersChild else 0
          if(motherSize>2)
            outputNonTerm("M" + motherSizeDesc + "C" + mothersChildDesc + perm)
          else
            outputNonTerm("M" + motherSizeDesc + "C" +        0     + perm)
        }
      }
    }
    outputNonTerm.lock()
    outputNonTerm
  }
  
  def parse(sent:List[String], a:Set[(Int, Int)], g:Grammar, markChildOrder:Boolean = false) : Chart = {
    
    val n = sent.size
    
    val chart = ChartHelper.emptyChart(n)
    
    for(attachSide <- Set(true, false)){
      for(attachHeight <- Set(true, false)){
        val tree = AlignmentCanonicalParser.parse(sent.size, a, attachSide, attachHeight)
        val initMothersChild = 0
        val initMotherSize = if(markChildOrder) 2 else 0
        addToChart(chart, tree, sent, initMotherSize, initMothersChild, g, markChildOrder)
      }
    }
    
    addStartSymbols(chart, g)

    chart
  }
  
  private def addStartSymbols(chart:Chart, g:Grammar) : Unit = {
    val start = 0
    val end = chart.size - 1
    val lhs = g.ROOT
    val rootNonTermSpan = new NonTermSpan()
    chart(start)(end).putIfAbsent(lhs, rootNonTermSpan)
    val it = chart(start)(end).iterator()
    while(it.hasNext()){
      it.advance()
      val nonTermSpan = it.value()
      if(! nonTermSpan.edges.isEmpty){
        val rhs = it.key()
        for(latentLhs <- g.latentMappings(lhs)){
          rootNonTermSpan.addEdge(Edge(start, end, g.getInnerRule(lhs, List(rhs)), List()))
        }
      }
    }
  }
  
  private def opToStr(op:List[Int], motherSize:Int, mothersChild:Int) : String = {
    if(motherSize>2)
      "M"+motherSize+"C"+mothersChild+"P"+op.mkString("")
    else
      "M"+motherSize+"C"+0+"P"+op.mkString("")
  }
  
  /**
   * this method is called ONLY in cases of many binary splits
   */
  private def partialNodeListToStr(parentOp:String, nodeList:List[TreeNode], markChildOrder:Boolean) : String = {
    if(nodeList.size>1){
      parentOp
    }else{
      if(markChildOrder){
        nodeToStr(nodeList.head, 2, 0)
      }else{
        nodeToStr(nodeList.head, 0, 0)
      }
    }
  }
  
  private def nodeToStr(node:TreeNode, motherSize:Int, mothersChild:Int) : String = {
    node match{
        case NonTerm(start, end, min, max, operator, children) =>
          opToStr(operator, motherSize, mothersChild)
        case Term(pos, el) => 
          if(motherSize>2){
            if(el == -1)
              "M"+motherSize+"C"+mothersChild+"N"
            else
              "M"+motherSize+"C"+mothersChild+"A"
          }else{
            if(el == -1)
              "M"+motherSize+"C"+0+"N"
            else
              "M"+motherSize+"C"+0+"A"
          }
      }
  }
  
  private def addToChart(chart:Chart, tree:TreeNode, words:List[String], motherSize:Int, mothersChild:Int, g:Grammar, markChildOrder:Boolean) : Unit = {
    val collapsedTree = HelperFunctions.collapseTree(tree)
    
    collapsedTree match {

      case Term(pos, el) =>
        val lhsStr = nodeToStr(tree, motherSize, mothersChild)
        val lhs = g.nonTerms(lhsStr)
        val rules = g.getAllLatentPretermRules(lhs, g.voc(words(pos)))
        val edges = rules.map{rule => Edge(pos, pos, rule, List())}

        storeEdges(chart, edges)

      case NonTerm(start, end, min, max, op, children) => {
        // recursion
        children.zipWithIndex.foreach{ case (child, index) =>
          val subMothersChild = if(markChildOrder) index+1 else 0
          val subMotherSize   = if(markChildOrder) op.size else 0
          addToChart(chart, child, words, subMotherSize, subMothersChild, g, markChildOrder)
        }

        val lhsStr = opToStr(op, motherSize, mothersChild)
        val lhs    = g.nonTerms(lhsStr)

        if(op.size==2){
          for(span <- 2 to children.size){
            for(spannedChildren <- children.sliding(span)){
              val spanStart = spannedChildren.head.left
              val spanEnd   = spannedChildren.last.right
              for(splitPoint <- spannedChildren.tail.map{_.left}){
                val (leftNodes, rightNodes) = spannedChildren.partition{_.left<splitPoint}
                val rhs = List(partialNodeListToStr(lhsStr, leftNodes, markChildOrder), partialNodeListToStr(lhsStr, rightNodes, markChildOrder)).map{g.nonTerms(_)}
                val rules = g.getAllLatentInnerRules(lhs, rhs)
                val edges = rules.map{rule => Edge(spanStart,  spanEnd, rule, List(splitPoint))}
                storeEdges(chart, edges)
              }
            }
          }
        }else{
          val splitPoints = children.tail.map(_.left)
          val rhs = children.zipWithIndex.map{ case (child, index) =>
            val subMothersChild = if(markChildOrder) index+1       else 0
            val subMotherSize   = if(markChildOrder) children.size else 0
            g.nonTerms(nodeToStr(child, subMotherSize, subMothersChild))
          }
          val rules = g.getAllLatentInnerRules(lhs, rhs)
          val edges = rules.map{rule => Edge(start, end, rule, splitPoints)}
          storeEdges(chart, edges)
        }
      }
    }
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
  
}