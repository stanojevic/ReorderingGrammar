package grammar.reordering.parser

import grammar.reordering.representation.Probability
import grammar.reordering.representation.Probability.LogNil
import java.io.PrintWriter
import java.io.File
import grammar.reordering.representation.{Rule, InnerRule, PretermRule}
import grammar.reordering.representation.Grammar

case class SimpleTreeNode (
    label:String,
    p:Probability,
    subTreeP:Probability,
    children:List[SimpleTreeNode],
    span:(Int, Int)
    ) {
  
  override
  def toString() : String = {
    // oversimplified Penn output format
    // without converting back unknown words
    // val spanStr = "["+span._1+":"+span._2+"]"
    val spanStr = ""
    if(children.size == 0){
      spanStr+label
    }else{
      val childrenString = children.map{_.toString}.mkString(" ")
      spanStr+"("+label+" "+childrenString+" )"
    }
  }
  
  def yieldOriginalSentence() : List[String] = {
    if(children.isEmpty){
      List(label)
    }else{
      children.flatMap(_.yieldOriginalSentence)
    }
  }
  
  private val UNALIGNED_SHIFT = 10000
  
  def yieldPermutationWithUnaligned() : List[Int] = {
    if(span._1 == span._2){
      // terminals
      List(span._1)
    }else if(children.size == 1){
      // for unarys with *1 *2... and preterminals
      children(0).yieldPermutationWithUnaligned
    }else{
      val childrenPerms = children.map{_.yieldPermutationWithUnaligned}
      if(label startsWith "P01"){
        val unaligned = childrenPerms(0).map{i =>
          if(i<0)
            i
          else
            i-UNALIGNED_SHIFT
        }
        val aligned = childrenPerms(1)
        unaligned ++ aligned
      }else if(label startsWith "P10"){
        val aligned = childrenPerms(0)
        val unaligned = childrenPerms(1).map{i =>
          if(i<0)
            i
          else
            i-UNALIGNED_SHIFT
        }
        aligned ++ unaligned
      }else if(label startsWith "P12"){
        childrenPerms.flatten
      }else if(label startsWith "P21"){
        childrenPerms.reverse.flatten
      }else if(label startsWith "P"){
        val parts = label split "_"
        val perm = parts(0).drop(1).map{_.toString.toInt}.toList
        perm.flatMap{i => childrenPerms(i-1)}
      }else{
        childrenPerms.flatten
      }
    }
  }
  
  def yieldReorderedWithoutUnaligned(sent:List[String]=null) : List[String] = {
    val workingSent:List[String] = if(sent == null){
      this.yieldOriginalSentence()
    }else{
      sent
    }

    val p = this.yieldPermutationWithUnaligned()
    p.filter{_>=0}.map{workingSent(_)}
  }
  
  def yieldReorderedWithUnaligned(sent:List[String]=null) : List[String] = {
    val workingSent:List[String] = if(sent == null){
      this.yieldOriginalSentence()
    }else{
      sent
    }

    val p = this.yieldPermutationWithUnaligned()
    p.map{ i =>
      if(i>=0){
        workingSent(i)
      }else{
        "UNALIGNED_"+workingSent(i+UNALIGNED_SHIFT)+"_UNALIGNED"
      }
    }
  }
  
  def toPennString(sent:List[String], depth:Int=0) : String = {
    if(children.size == 0){
      sent(span._1)
    }else{
      val childrenString = children.map{_.toPennString(sent, depth+1)}.mkString(" ")
      "("+label+" "+childrenString+")"
    }
  }
  
  def toPennStringIndented(sent:List[String], depth:Int=0) : String = {
    val indentation = "  "*depth
    if(span._1 == span._2){
      indentation+sent(span._1)
    }else{
      val childrenString = children.map{_.toPennStringIndented(sent, depth+1)}.mkString("\n")
      indentation+"("+label+"\n"+childrenString+")"
    }
  }
  
  def toDotString(sent:List[String], graphLabel:String) : String = {
    val colorMapping = Map[String, String](
        "P12" -> "green3",
        "P21" -> "firebrick1"
    ).withDefaultValue("blue")
    val terminalColor = "lightblue2"
  
    def toDotStringRec(node:SimpleTreeNode, nodeId:String) : (String, List[String]) = {
      var outStr = ""
        
      val parts = node.label.split("_")
      var terms = List[String]()
      
      outStr += nodeId+"[label=\""+node.label+"\"; "
      outStr += "fontcolor="+colorMapping(parts(0))+"; style=bold];\n"
      node.children.zipWithIndex.foreach{ case (child, index) =>
        val childName = nodeId+index
        if(child.children.size > 0){
          val res = toDotStringRec(child, childName)
          outStr += res._1
          terms ++= res._2
        }else{
          terms ++= List(childName)
        }
        val style = 
          if(    (parts(0)=="P01" && index == 0)
              || (parts(0)=="P10" && index == 1
              || (parts(0)=="P01*1")
              || (parts(0)=="P10*2")            )){
            "[style=dotted]"
          }else{
            ""
          }
        outStr += s"$nodeId -- $childName $style ;\n"
      }
        
      (outStr, terms)
    }

    var outStr = ""
      
    outStr += "graph { \n"
    outStr += "  label=\""+graphLabel+"\"\n"
    
    val res = toDotStringRec(this, "node0")
    outStr += res._1
    val terms = res._2
    
    outStr += "  subgraph {rank=same;\n"
    terms.zipWithIndex.foreach{ case (nodeId, i) =>
      val word = sent(i)
      outStr += "    "+nodeId+"[shape=box; "
      outStr += "label=\""+word+s" ($i)"+"\" "
      outStr += "style=bold; "
      outStr += "color="+terminalColor+"];\n"
    }
    outStr += "    edge[style=\"invis\"];\n"

    outStr += "  }\n"
    outStr += "}\n"
      
    outStr
  }
  
  def visualize(sent:List[String] = null, graphLabel:String="graph") : Unit = {
    val workingSent:List[String] = if(sent == null){
      this.yieldOriginalSentence()
    }else{
      sent
    }
    val dotString = this.toDotString(workingSent, graphLabel)

    val file = File.createTempFile("visual", ".dot", null)
    file.deleteOnExit()
    val tmpFileName = file.getPath()
    val pw = new PrintWriter(file)
    pw.println(dotString)
    pw.close()
    
    val dotCmd = s"dot -Tpng $tmpFileName -O"
    val pDot = Runtime.getRuntime().exec(dotCmd);
    pDot.waitFor()
    new File(s"$tmpFileName.dot.png").deleteOnExit()

    val xdgCmd = System.getProperty("os.name") match {
      case "Linux" => s"nohup xdg-open $tmpFileName.png"
      case _       => s"open $tmpFileName.dot.png"
    }
    val pXdg = Runtime.getRuntime().exec(xdgCmd);
    val seconds = 5
    Thread.sleep(seconds*1000)
  }
  
  private var memoizedRules : List[Rule] = null
  
  def extractRules(g:Grammar) : List[Rule] = {
    if(memoizedRules == null){
      if(children.size == 0){
        memoizedRules = List()
      }else if(children(0).children.size == 0){
        val lhs = g.nonTerms(label)
        val word = g.voc(children(0).label)
        memoizedRules = List(PretermRule(lhs, word, p))
      }else{
        val lhs = g.nonTerms(label)
        val rhs = children.map{kid:SimpleTreeNode => g.nonTerms(kid.label)}
        val rule = InnerRule(lhs, rhs, p)
        val subRules = children.flatMap{_.extractRules(g)}
        memoizedRules = rule::subRules
      }
    }
    memoizedRules
  }

}

object SimpleTreeNode {
  
  def fromPennString(s:String) : SimpleTreeNode = {
    val tokens = tokenizePennString(s)
    
    parsePennRec(tokens, 0)._1
  }
  
  def parsePennRec(tokens:List[String], spanStart:Int) : (SimpleTreeNode, List[String]) = {
    if(tokens.head == "("){
      val label = tokens.tail.head
      var workingTokens = tokens.tail.tail
      var workingSpanStart = spanStart
      var children = List[SimpleTreeNode]()
      while(workingTokens.head != ")"){
        val (node, leftOver) = parsePennRec(workingTokens, workingSpanStart)
        children ::= node
        workingSpanStart = node.span._2+1
        workingTokens = leftOver
      }

      children = children.reverse
      
      val node = SimpleTreeNode(
        label = label,
        p = LogNil,
        subTreeP = LogNil,
        children = children,
        span = (spanStart, children.last.span._2)
      )
      (node, workingTokens.tail)
    }else{
      val label = tokens.head
      val node = SimpleTreeNode(
        label = label,
        p = LogNil,
        subTreeP = LogNil,
        children = List(),
        span = (spanStart, spanStart)
      )
      (node, tokens.tail)
    }
  }
  
  def tokenizePennString(s:String) : List[String] = {
    val s1 = s.replaceAllLiterally("(", "( ")
    val s2 = s1.replaceAllLiterally(")", " )")
    val tokens = s2.split("\\s+").toList
    tokens.filterNot{_.matches("^\\s*$")}
  }

}
