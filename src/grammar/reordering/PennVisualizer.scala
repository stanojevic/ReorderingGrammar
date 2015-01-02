package grammar.reordering

import grammar.reordering.parser.SimpleTreeNode

object PennVisualizer {
  
  def main(argv:Array[String]) : Unit = {
    var input = readLine("enter the penn tree: ")
    while(input != "" && input.toUpperCase() != "EXIT"){
      val tree = SimpleTreeNode.fromPennString(input)
      tree.visualize()
      input = readLine("enter the penn tree: ")
    }
    println("exiting")
  }

}