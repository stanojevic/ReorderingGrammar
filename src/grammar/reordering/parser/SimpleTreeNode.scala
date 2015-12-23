package grammar.reordering.parser

import grammar.reordering.representation.Probability
import grammar.reordering.representation.Probability.{LogNil, LogOne}
import java.io.PrintWriter
import java.io.File
import grammar.reordering.representation.{Rule, InnerRule, PretermRule}
import grammar.reordering.representation.Grammar

case class SimpleTreeNode (
    label:String,
    score:Double, // by default it's log of probability but it can be something else too
    subTreeScore:Double,
    children:List[SimpleTreeNode],
    span:(Int, Int)
    ) {
  
  private lazy val hash = (label, children, span).hashCode()

  override
  def hashCode() = hash
  
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
  
  def yieldPermutationWithUnaligned() : List[Int] = yieldPermutationWithUnaligned_memo
  private lazy val yieldPermutationWithUnaligned_memo = this.yieldPermutationWithUnaligned_impl()
  private def yieldPermutationWithUnaligned_impl() : List[Int] = {
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
            i-SimpleTreeNode.UNALIGNED_SHIFT
        }
        val aligned = childrenPerms(1)
        unaligned ++ aligned
      }else if(label startsWith "P10"){
        val aligned = childrenPerms(0)
        val unaligned = childrenPerms(1).map{i =>
          if(i<0)
            i
          else
            i-SimpleTreeNode.UNALIGNED_SHIFT
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

    val p = this.yieldPermutationWithUnaligned
    p.filter{_>=0}.map{workingSent(_)}
  }
  
  def yieldReorderedWithUnaligned(sent:List[String]=null) : List[String] = {
    val workingSent:List[String] = if(sent == null){
      this.yieldOriginalSentence()
    }else{
      sent
    }

    val p = this.yieldPermutationWithUnaligned
    p.map{ i =>
      if(i>=0){
        workingSent(i)
      }else{
        "UNALIGNED_"+workingSent(i+SimpleTreeNode.UNALIGNED_SHIFT)+"_UNALIGNED"
      }
    }
  }
  
  def toPennString(sent:List[String]=null, depth:Int=0, printDephrased:Boolean=false) : String = {
    if(children.size == 0){
      val bigPhrase = if(sent == null) this.label else sent(span._1)
      val output = if(bigPhrase.startsWith("[[[") && bigPhrase.endsWith("]]]") && bigPhrase.contains("___")){
        bigPhrase.substring(3, bigPhrase.length-3).split("___").map(SimpleTreeNode.escapeBrackets).mkString(" ")
      }else{
        SimpleTreeNode.escapeBrackets(bigPhrase)
      }
      output
    }else{
      val childrenString = children.map{_.toPennString(sent, depth+1)}.mkString(" ")
      "("+SimpleTreeNode.escapeBrackets(label)+" "+childrenString+")"
    }
  }
  
  def toPennStringIndented(sent:List[String]=null, depth:Int=0) : String = {
    val indentation = "  "*depth
    if(children.size == 0){
      val bigPhrase = if(sent == null) this.label else sent(span._1)
      val output = if(bigPhrase.startsWith("[[[") && bigPhrase.endsWith("]]]") && bigPhrase.contains("___")){
        bigPhrase.substring(3, bigPhrase.length-3).split("___").map(SimpleTreeNode.escapeBrackets).mkString(" ")
      }else{
        SimpleTreeNode.escapeBrackets(bigPhrase)
      }
      indentation+output
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
    val shapeMapping = Map[String, String](
        "ROOT" -> "ellipse",
        "P12" -> "ellipse",
        "P21" -> "hexagon",
        "P10" -> "ellipse",
        "P01" -> "ellipse",
        "P3142" -> "ellipse",
        "P2413" -> "ellipse",
        "P35142" -> "ellipse",
        "P24153" -> "ellipse"
    ).withDefaultValue("plaintext")

    val terminalColor = "lightblue2"
  
    def toDotStringRec(node:SimpleTreeNode, nodeId:String) : (String, List[String]) = {
      var outStr = ""
        
      val parts = node.label.split("_")
      var terms = List[String]()
      
      outStr += nodeId+"[label=<<B>"+node.label+"</B>>; "
      val shape = if(parts(0) startsWith "tag_") "square" else shapeMapping(parts(0))
      outStr += "shape="+shape+"; "
      // outStr += "fontcolor="+colorMapping(parts(0))+"; "
      outStr += "color="+colorMapping(parts(0))+"; "
      // outStr += "fontname=\"Times-Bold\" ; "
      val fontSize = if(parts(0) contains "*") 10 else 30
      outStr += "fontsize="+fontSize+"; "
      outStr += "style=bold];\n"
      node.children.zipWithIndex.foreach{ case (child, index) =>
        val childName = nodeId+"_"+index
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
    if(graphLabel != ""){
      outStr += "  label=<<B>"+graphLabel+"</B>>\n"
    }
    
    val res = toDotStringRec(this, "node0")
    outStr += res._1
    val terms = res._2
    
    outStr += "  subgraph {rank=same;\n"
    terms.zipWithIndex.foreach{ case (nodeId, i) =>
      val word = sent(i)
      outStr += "    "+nodeId+"[shape=plaintext; "
      // outStr += "label=<<B>"+word+s" ($i)"+"</B>> "
      outStr += "label=<<B>"+word+"</B>> "
      outStr += "fontsize=30 "
      outStr += "style=bold; "
      outStr += "color="+terminalColor+"];\n"
    }
    outStr += "    edge[style=\"invis\"];\n"

    outStr += "  }\n"
    outStr += "}\n"
      
    outStr
  }
  
  def visualize(sent:List[String] = null, graphLabel:String="") : Unit = {
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
        val p = if(score > 0) Probability (score) else new Probability(score)
        memoizedRules = List(PretermRule(lhs, word, p))
      }else{
        val lhs = g.nonTerms(label)
        val rhs = children.map{kid:SimpleTreeNode => g.nonTerms(kid.label)}
        val p = if(score > 0) Probability (score) else new Probability(score)
        val rule = InnerRule(lhs, rhs, p)
        val subRules = children.flatMap{_.extractRules(g)}
        memoizedRules = rule::subRules
      }
    }
    memoizedRules
  }
  
  def deuniarize() : SimpleTreeNode = {
    val newChildren = if(
        this.children.size>1 &&
        ! this.label.startsWith("tag_") &&
        this.children.forall(_.children.size == 1)){
      this.children.map{child => child.children.head.deuniarize}
    }else{
      this.children.map{child => child.deuniarize}
    }
    SimpleTreeNode(this.label, this.score, this.subTreeScore, newChildren, this.span)
  }
  
  def flatten() : SimpleTreeNode = {
    if(this.children.isEmpty){
      this
    }else{
      var newChildren = this.children
      val opLabel = if(this.label.contains("_") && ! this.label.startsWith("tag_")){
        this.label.substring(0, this.label.indexOf("_"))
      }else{
        this.label
      }
      if(this.label.contains("P12") || this.label.contains("P21")){
        while(newChildren.exists{child => child.label.contains(opLabel)}){
          newChildren = newChildren.flatMap{ child =>
            if(child.label.contains(opLabel)){
              child.children
            }else{
              List(child)
            }
          }
        }
      }else if(this.label.startsWith("P")){
        // some n-ary operator
        newChildren = newChildren.flatMap{ child => child.children}
      }
      newChildren = newChildren.map{_.flatten}
      
      SimpleTreeNode(opLabel, this.score, this.subTreeScore, newChildren, this.span)
    }
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
        label = unescapeBrackets(label),
        score = LogOne.log,
        subTreeScore = LogOne.log,
        children = children,
        span = (spanStart, children.last.span._2)
      )
      (node, workingTokens.tail)
    }else{
      val label = tokens.head
      val node = SimpleTreeNode(
        label = unescapeBrackets(label),
        score = LogOne.log,
        subTreeScore = LogOne.log,
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

  private def escapeBrackets(label:String) = {
    label.replaceAllLiterally("(", "-LRB-").
          replaceAllLiterally(")", "-RRB-")
  }

  private def unescapeBrackets(label:String) = {
    label.replaceAllLiterally("-LRB-", "(").
          replaceAllLiterally("-RRB-", ")")
  }
  
  val UNALIGNED_SHIFT = 10000
  
}
