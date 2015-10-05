package grammar.reordering.parser.metric

import grammar.reordering.parser.SimpleTreeNode

trait MetricFunction {
  
  def score(sys:SimpleTreeNode, ref:SimpleTreeNode) : Double

}