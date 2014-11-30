package grammar.reordering.EM

import beer.permutation.pet.representation.TreeNode
import beer.permutation.pet.representation.NonTerm
import beer.permutation.pet.representation.Term
import java.io.PrintStream
import java.io.File

object AlignmentCanonicalParser {
  
  type Alignment = Set[(Int, Int)]
  
  def extractAlignment(s:String) : Alignment =
    s.split(" ").toSet.map{ entry:String =>
      val fields = entry split "-"
      (fields(0).toInt, fields(1).toInt)
    }
  
  def parse(n:Int, a:Alignment, leftSide:Boolean, lowest:Boolean) : beer.permutation.pet.representation.TreeNode = {
    val avgPositions = avgTargetPosition(n, a)

    val minPermutation:List[Int] = findMinPermutation(avgPositions.toList) map {_+1}
    
    val tree = beer.permutation.pet.parser.ShiftReduce.parse(minPermutation)
    
    val mapping = new Array[Int](n)
    avgPositions.zipWithIndex.filter{_._1>=0}.map{_._2}.zip(minPermutation).foreach{ case (index, permIndex) =>
      mapping(index) = permIndex
    }
    val inverseMapping = new Array[Int](n+1) // adding one because permutations start from 1 and we ignore 0th element
    for(i <- 0 until mapping.size){
      if(mapping(i) != 0)
        inverseMapping(mapping(i)) = i
    }

    val repairedTree = insertUnalignedAndConvert(mapping, inverseMapping, tree, lowest, leftSide)

    repairedTree
  }
  
  def insertUnalignedAndConvert(mapping:Array[Int], inverseMapping:Array[Int], tree: TreeNode, lowest:Boolean, leftSide:Boolean) : TreeNode = {
    val n = mapping.size
    
    tree match {
      case NonTerm(start:Int, end:Int, min:Int, max:Int, operator:List[Int], children:List[TreeNode]) =>
        val newChildren = children map {insertUnalignedAndConvert(mapping, inverseMapping, _, lowest, leftSide)}

        if(!lowest){ // highest attachment
          if(leftSide){
            val newNewChildren = newChildren.init.map{ child:TreeNode =>
              val unalignedNeighbours = (child.right+1 to n).takeWhile{mapping(_) == 0}
              unalignedNeighbours.foldLeft(child){case (acc, unaligned) =>
                NonTerm(acc.left, acc.right+1, -1, -1, List(1, 0), List(acc, Term(unaligned, -1)))
              }
            } ++ List(newChildren.last)
            NonTerm(newNewChildren.head.left, newNewChildren.last.right, min, max, operator, newNewChildren)
          }else{ // rightSide
            val newNewChildren = newChildren.head :: newChildren.tail.map{ child:TreeNode =>
              val unalignedNeighbours = (child.left-1  to 0 by -1).takeWhile{mapping(_) == 0}
              unalignedNeighbours.foldLeft(child){case (acc, unaligned) =>
                NonTerm(acc.left-1, acc.right, -1, -1, List(0, 1), List(Term(unaligned, -1), acc))
              }
            }
            NonTerm(newNewChildren.head.left, newNewChildren.last.right, min, max, operator, newNewChildren)
          }
        }else{
          NonTerm(newChildren.head.left, newChildren.last.right, min, max, operator, newChildren)
        }
      case Term(position:Int, el:Int) =>
        val realIndex = inverseMapping(el)
        var child:TreeNode = Term(realIndex, realIndex)
        
        val leftMostAligned = mapping.zipWithIndex.dropWhile(_._1 == 0).head._2
        val rightMostAligned = mapping.zipWithIndex.reverse.dropWhile(_._1 == 0).head._2
        
        if(realIndex == leftMostAligned && realIndex > 0){
          val unalignedNeighbours = (realIndex-1 to 0 by -1).takeWhile{mapping(_) == 0}
          child = unalignedNeighbours.foldLeft(child.asInstanceOf[TreeNode]){case (acc, unaligned) =>
            NonTerm(acc.left-1, acc.right, -1, -1, List(0, 1), List(Term(unaligned, -1), acc))
          }
        }

        if(realIndex == rightMostAligned && realIndex < n-1){
          val unalignedNeighbours = (realIndex+1 until n).takeWhile{mapping(_) == 0}
          child = unalignedNeighbours.foldLeft(child.asInstanceOf[TreeNode]){case (acc, unaligned) =>
            NonTerm(acc.left, acc.right+1, -1, -1, List(1, 0), List(acc, Term(unaligned, -1)))
          }
        }
        
        if(lowest){
          if(leftSide && rightMostAligned != realIndex){
            val unalignedNeighbours = (realIndex+1 until n).takeWhile{mapping(_) == 0}
            unalignedNeighbours.foldLeft(child.asInstanceOf[TreeNode]){case (acc, unaligned) =>
              NonTerm(acc.left, acc.right+1, -1, -1, List(1, 0), List(acc, Term(unaligned, -1)))
            }
          }else if(!leftSide && leftMostAligned != realIndex){ // rightSide
            val unalignedNeighbours = (realIndex-1 to 0 by -1).takeWhile{mapping(_) == 0}
            unalignedNeighbours.foldLeft(child.asInstanceOf[TreeNode]){case (acc, unaligned) =>
              NonTerm(acc.left-1, acc.right, -1, -1, List(0, 1), List(Term(unaligned, -1), acc))
            }
          }else{
            child
          }
        }else{
          child
        }
    }

  }
  
  private def opToStr(op:List[Int]) : String = {
    if(op == List(0,0)){
      "ROOT"
    }else{
      "<"+op.mkString(",")+">"
    }
  }
  
  private def printDotTree(pw:PrintStream, tree:TreeNode, sent:List[String], graphLabel:String, nodeId:String="node0") : List[(String, Term)] = {
    val colorMapping = Map[String, String](
        "<1,2>" -> "green3",
        "<2,1>" -> "firebrick1"
        ).withDefaultValue("blue")
    val terminalColor = "lightblue2"

    tree match {
      case NonTerm(start, end, _, _, operator, children) =>
        if(nodeId == "node0"){
          pw.println("graph { ")
          pw.println("  label=\""+graphLabel+"\"")
        }
        
        var terms = List[(String, Term)]()
    
        val opString = opToStr(operator)
        pw.println(nodeId+"[label=\""+opString+"\"; fontcolor="+colorMapping(opString)+"; style=bold];")
        children.zipWithIndex.foreach{ case (child, index) =>
          val childName = nodeId+index
          child match {
            case NonTerm(start, end, _, _, operator, children) =>
              terms ++= printDotTree(pw, child, sent, graphLabel, childName)
              pw.println(nodeId+" -- "+childName+" ;")
            case term @ Term(position, el) =>
              terms ++= List((childName, term))
              if(operator(index) == 0){
                pw.println(nodeId+" -- "+childName+" [style=dotted] ;")
              }else{
                pw.println(nodeId+" -- "+childName+" ;")
              }
          }
        }

        if(nodeId == "node0"){
          pw.println("subgraph {rank=same;")
          terms.foreach{ case (termName, Term(pos, el)) =>
            var style = " style="
            if(el == -1){
              style += "dotted"
            }else{
              style += "bold"
            }
            val word:String = "\""+sent(pos)+s" ($pos)"+"\""
            pw.println("  "+termName+"[shape=box; label="+word+style+"; color="+terminalColor+"];")
            // pw.println("  term"+(pos+1)+"[shape=box; label="+el+"; color="+terminalColor+"];")
          }
          pw.println("edge[style=\"invis\"];")
          // pw.println("  "+(1 to terms.size).map{"term"+_}.mkString(" -- ") + " ; ")
          pw.println("} ")
          pw.println("} ")
        }

        terms
      case term @ Term(position, el) =>
        List((nodeId, term))
    }
    
  }

  def visualizeTree(tree:TreeNode, label:String) : Unit = {
    val n = tree.count(_.isInstanceOf[Term])
    val fakeSent = (0 to 2*n).toList.map{_.toString}
    visualizeTree(tree, fakeSent, label)
  }

  def visualizeTree(tree:TreeNode, sent:List[String], label:String) : Unit = {
    val file = File.createTempFile("visual", "")
    file.deleteOnExit()
    val tmpFileName = file.getPath()
    val pw = new PrintStream(s"$tmpFileName.dot")
    println(s"DOT FILE $tmpFileName.dot")
    printDotTree(pw, tree, sent, label)
    pw.close()

    val dotCmd = s"dot -Tpng $tmpFileName.dot -O"
    val pDot = Runtime.getRuntime().exec(dotCmd);
    pDot.waitFor()
    new File(s"$tmpFileName.dot.png").deleteOnExit()

    val xdgCmd = System.getProperty("os.name") match {
      case "Linux" => s"nohup xdg-open $tmpFileName.dot.png"
      case _       => s"open $tmpFileName.dot.png"
    }
    val pXdg = Runtime.getRuntime().exec(xdgCmd);
  }
  
  def findMinPermutation(avgPositions:List[Double]) : List[Int] = {
    val nonNullAlignments = avgPositions.filter(_>=0)
    
    val positioning = (0 until nonNullAlignments.size).toList.sortWith{ case (i:Int, j:Int) =>
      if(nonNullAlignments(i) < nonNullAlignments(j)){
        true
      }else if(nonNullAlignments(i) > nonNullAlignments(j)){
        false
      }else{
        if(i<j)
          true
        else
          false
      }
    }
    
    val permutation = new Array[Int](positioning.size)
    for(i <- 0 until positioning.size){
      permutation(positioning(i)) = i
    }
    permutation.toList
  }
  
  def avgTargetPosition(n:Int, a:Alignment) : Array[Double] = {
    val avgPositions = Array.fill(n)(0.0)
    val alignmentLinks = Array.fill(n)(0)
    a.foreach{ case (src, tgt) =>
      avgPositions(src)   += tgt
      alignmentLinks(src) += 1
    }
    for(i <- 0 until n){
      if(alignmentLinks(i)>0){
        avgPositions(i) /= alignmentLinks(i)
      }else{
        avgPositions(i) = -1
      }
    }
    avgPositions
  }

}