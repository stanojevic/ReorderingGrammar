package grammar.reordering.parser

import beer.permutation.pet.representation.{
                                                        TreeNode,
                                             Term    => TreeTerm,
                                             NonTerm => TreeNonTerm
                                           }

object Yield {
  
  def yieldTree(tree:TreeNode, s:List[String]) : List[String] = {
    permuteString(treeToPermutation(tree), s)
  }
  
  def yieldTreeWithUnaligned(tree:TreeNode, s:List[String]) : List[String] = {
    treeToPermutation(tree).map{ index =>
      if(index >=0 ){
        s(index)
      }else{
        "UNALIGNED_"+s(index+1000)
      }
    }
  }
  
  def filterOutUnaligned(quasiPerm:List[Int]) : List[Int] = quasiPerm.filter{_ >= 0}
  
  def permuteString(quasiPerm:List[Int], s:List[String]) : List[String] = {
    filterOutUnaligned(quasiPerm).map{s(_)}
  }
  
  def treeToPermutation(node:TreeNode) : List[Int] = node match {
    case TreeTerm(pos, el) =>
      if(el < 0){
        List(pos-1000)
      }else{
        List(pos)
      }
    case TreeNonTerm(start, end, _, _, operator, children) =>
      val subPerms:List[List[Int]] = children.map{treeToPermutation}

      if(operator == List(0, 0)){
        subPerms.flatten
      }else if(operator == List(1, 2)){
        subPerms.flatten
      }else if(operator == List(2,1)){
        subPerms.reverse.flatten
      }else if(operator(0) == 0){ // don't need to do anything if there are unaligned Inner stuff
        // number 1000 is chosen arbitrarily since no sentence will be of that size
        if(subPerms.size > 1){
          (subPerms(0).map{ el => if(el>=0) el-1000 else el })++subPerms(1)
        }else{
          (subPerms(0).map{ el => if(el>=0) el-1000 else el })
        }
      }else if(operator.size > 1 && operator(1) == 0){
        subPerms(0)++(subPerms(1).map{ el => if(el>=0) el-1000 else el })
      }else{
        val reorderedSubPerms:List[List[Int]] = if(subPerms.size == 1 && operator.size >2){
          subPerms
        }else{
          operator.map{ pos:Int =>
            // if(subPerms.size-1 < pos-1){
            //  System.err.println("problem maker : "+operator)
            //  System.err.println("subPerms.size : "+subPerms.size)
            //  System.err.println("subPerms : "+subPerms)
            // }
            subPerms(pos-1)
          }
        }
        
        reorderedSubPerms.flatten
      }
  }

  def pennTree(tree:TreeNode, sent:List[String], depth:Int=0) : String = {
    var result = ""
    for(i <- 0 until depth)
      result += "  "
    tree match {
      case TreeNonTerm(start, end, min, max, operator, children) =>
        result+="("+opToStr(operator)+"\n"
        for(child <- children)
          result+=pennTree(child, sent, depth+1)
        for(i <- 0 until depth)
          result+="  "
        result+=")"
      case TreeTerm(position, el) =>
        val word = sent(position)
        result+=s"[$word]"
    }
    result+="\n"
    result
  }

  private def opToStr(op:List[Int]) : String = {
    "<"+op.mkString(",")+">"
  }

}