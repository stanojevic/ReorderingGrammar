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
  
  def filterOutUnaligned(quasiPerm:List[Int]) : List[Int] = quasiPerm.filter{_ >= 0}
  
  def permuteString(quasiPerm:List[Int], s:List[String]) : List[String] = {
    filterOutUnaligned(quasiPerm).map{s(_)}
  }
  
  def treeToPermutation(node:TreeNode) : List[Int] = node match {
    case TreeTerm(pos, el) =>
      if(el < 0){
        List(-el)
      }else{
        List(pos)
      }
    case TreeNonTerm(start, end, _, _, operator, children) =>
      val subPerms:List[List[Int]] = children.map{treeToPermutation}

      if(operator contains 0){ // don't need to do anything if there are unaligned Inner stuff
        subPerms.flatten
      }else if(operator == List(1, 2)){
        subPerms.flatten
      }else if(operator == List(2,1)){
        subPerms.reverse.flatten
      }else{
        val reorderedSubPerms:List[List[Int]] = operator.map{ pos:Int =>
            subPerms(pos-1)
        }
        
        reorderedSubPerms.flatten
      }
  }

}