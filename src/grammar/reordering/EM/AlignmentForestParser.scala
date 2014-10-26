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
      for(perm <- List("A", "N", "P01", "P10", "P12", "P21", "P2413", "P3142", "P24153", "P35142")){
        if(motherSize>2)
          defaultNonTerms("M"+motherSize+"C"+mothersChild+perm)
        else
          defaultNonTerms("M"+motherSize+"C"+0+perm)
      }
    }
  }
  defaultNonTerms.lock()
  
  val defaultLatentMappings = defaultNonTerms.allInts.map{ x => x->List(x)}.toMap
  
  def parse(sent:List[String], a:Set[(Int, Int)], g:Grammar) : Chart = {
    
    val n = sent.size
    
    val chart = ChartHelper.emptyChart(n, g.nonTerms)
    
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
    val lhs = g.nonTerms("ROOT")
//    val edges:Set[Edge] = chart(start)(end).
//      filter{ case (k:grammar.reordering.representation.NonTerm, v:NonTermSpan) => v.edges.size>0}.
//      flatMap{ case (key, value) => g.getAllLatentInnerRules(lhs, List(key)).map{Edge(start, end, _, List())}}.toSet
    val edges:Set[Edge] = chart(start)(end).filter(_._2.edges.size>0).keySet.flatMap{ rhs =>
      g.latentMappings(lhs).map{latentLhs => Edge(start, end, g.getInnerRule(latentLhs, List(rhs)), List())}
    }
    for(latentLhs <- g.latentMappings(lhs)){
      chart(start)(end) += latentLhs -> NonTermSpan(edges.filter(_.rule.lhs == latentLhs))
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

        edges.groupBy(_.rule.lhs).foreach{ case (latentLhs, latentEdges:Set[Edge]) =>
          if(chart(pos)(pos) contains latentLhs){
            chart(pos)(pos)(latentLhs).addEdges(latentEdges)
          }else{
            chart(pos)(pos) += latentLhs -> NonTermSpan(latentEdges)
          }
        }

//        for(latentLhs <- g.latentMappings(lhs)){
//          if(chart(pos)(pos) contains latentLhs){
//            chart(pos)(pos)(latentLhs).addEdges(edges.filter(_.rule.lhs == latentLhs))
//          }else{
//            chart(pos)(pos) += latentLhs -> NonTermSpan(edges.filter(_.rule.lhs == latentLhs))
//          }
//        }

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
              var edges = Set[Edge]()
              for(splitPoint <- spannedChildren.tail.map{_.left}){
                val (leftNodes, rightNodes) = spannedChildren.partition{_.left<splitPoint}
                val rhs = List(partialNodeListToStr(lhsStr, leftNodes), partialNodeListToStr(lhsStr, rightNodes)).map{g.nonTerms(_)}
                val rules = g.getAllLatentInnerRules(lhs, rhs)
                edges ++= rules.map{rule => Edge(spanStart,  spanEnd, rule, List(splitPoint))}
              }

              edges.groupBy(_.rule.lhs).foreach{ case (latentLhs, latentEdges:Set[Edge]) =>
                if(chart(spanStart)(spanEnd) contains latentLhs){
                  chart(spanStart)(spanEnd)(latentLhs).addEdges(latentEdges)
                }else{
                  chart(spanStart)(spanEnd) += latentLhs -> NonTermSpan(latentEdges)
                }
              }
//              for(latentLhs <- g.latentMappings(lhs)){
//                if(chart(spanStart)(spanEnd) contains latentLhs){
//                  chart(spanStart)(spanEnd)(latentLhs).addEdges(edges.filter(_.rule.lhs == latentLhs))
//                }else{
//                  chart(spanStart)(spanEnd) += latentLhs -> NonTermSpan(edges.filter(_.rule.lhs == latentLhs))
//                }
//              }
            }
          }
        }else{
          val splitPoints = children.tail.map(_.left)
          val rhs = children.zipWithIndex.map{ case (child, index) => g.nonTerms(nodeToStr(child, children.size, index+1))}
          val rules = g.getAllLatentInnerRules(lhs, rhs)
          val edges = rules.map{rule => Edge(start, end, rule, splitPoints)}
          edges.groupBy(_.rule.lhs).foreach{ case (latentLhs, latentEdges:Set[Edge]) =>
            if(chart(start)(end) contains latentLhs){
              chart(start)(end)(latentLhs).addEdges(latentEdges)
            }else{
              chart(start)(end) += latentLhs -> NonTermSpan(latentEdges)
            }
          }
        }
      }
    }
  }
  
}