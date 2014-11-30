package grammar.reordering.EM

import beer.permutation.pet.representation.TreeNode
import beer.permutation.pet.parser.HelperFunctions
import beer.permutation.pet.representation.Term
import beer.permutation.pet.representation.NonTerm
import grammar.reordering.representation._

object AlignmentForestParser {
  
  val defaultNonTerms = new IntMapping()
  defaultNonTerms("ROOT")
  for(motherSize <- List(2, 4, 5)){
    for(mothersChild <- 1 to motherSize){
      for(perm <- List("A", "N", "P01", "P10", "P12", "P21", "P2413", "P3142", "P24153", "P25314", "P42513", "P41352", "P35142", "P31524")){
        defaultNonTerms(perm)
        if(motherSize>2)
          defaultNonTerms("M" + motherSize + "C" + mothersChild + perm)
        else
          defaultNonTerms("M" + motherSize + "C" +        0     + perm)
      }
    }
  }
  defaultNonTerms.lock()
  
  val defaultLatentMappings = defaultNonTerms.allInts.map{ x => x->List(x)}.toMap
  
  def parse(sent:List[String], a:Set[(Int, Int)], g:Grammar) : Chart = {
    
    val n = sent.size
    
    val chart = ChartHelper.emptyChart(n)
    
    for(attachSide <- Set(true, false)){
      for(attachHeight <- Set(true, false)){
        val tree = AlignmentCanonicalParser.parse(sent.size, a, attachSide, attachHeight)
        addToChart(chart, tree, sent, 2, 0, g)
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
  private def partialNodeListToStr(parentOp:String, nodeList:List[TreeNode]) : String = {
    if(nodeList.size>1)
      parentOp
    else
      nodeToStr(nodeList.head, 2, 0)
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
  
  private def addToChart(chart:Chart, tree:TreeNode, words:List[String], motherSize:Int, mothersChild:Int, g:Grammar) : Unit = {
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
        children.zipWithIndex.foreach{ case (child, index) => addToChart(chart, child, words, op.size, index+1, g)}

        val lhsStr = opToStr(op, motherSize, mothersChild)
        val lhs    = g.nonTerms(lhsStr)

        if(op.size==2){
          for(span <- 2 to children.size){
            for(spannedChildren <- children.sliding(span)){
              val spanStart = spannedChildren.head.left
              val spanEnd   = spannedChildren.last.right
              for(splitPoint <- spannedChildren.tail.map{_.left}){
                val (leftNodes, rightNodes) = spannedChildren.partition{_.left<splitPoint}
                val rhs = List(partialNodeListToStr(lhsStr, leftNodes), partialNodeListToStr(lhsStr, rightNodes)).map{g.nonTerms(_)}
                val rules = g.getAllLatentInnerRules(lhs, rhs)
                val edges = rules.map{rule => Edge(spanStart,  spanEnd, rule, List(splitPoint))}
                storeEdges(chart, edges)
              }
            }
          }
        }else{
          val splitPoints = children.tail.map(_.left)
          val rhs = children.zipWithIndex.map{ case (child, index) => g.nonTerms(nodeToStr(child, children.size, index+1))}
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