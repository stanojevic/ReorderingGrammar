package grammar.reordering

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class MonotonizeTest extends FlatSpec with ShouldMatchers{

  "reordering" should "not fail" in {
    val a = Set( (1,1), (1,2), (3,4), (6, 0))
    val s = List(
        "unaligned1",
        "aligned1",
        "unaligned2",
        "aligned2",
        "unaligned3",
        "unaligned4",
        "aligned3",
        "unaligned5"
        )

    val goodS_right = List(
        "unaligned3",
        "unaligned4",
        "aligned3",
        "unaligned5",
        "unaligned1",
        "aligned1",
        "unaligned2",
        "aligned2"
        )
    val goodA_right = Set( (5,1), (5,2), (7,4), (2, 0) )
    
    val goodS_left = List(
        "aligned3",
        "unaligned5",
        "unaligned1",
        "aligned1",
        "unaligned2",
        "aligned2",
        "unaligned3",
        "unaligned4"
        )
    val goodA_left = Set( (3,1), (3,2), (5,4), (0, 0) )
    
    
    for(attachLeft <- List(true, false)){
      val (newS, newA) = Monotonize.do_reordering(s, a, attachLeft)
      if(attachLeft){
        System.err.println(" newS: "+ newS.toList.sorted)
        System.err.println("goodS: "+goodS_left.toList.sorted)
        
        System.err.println(" newA: "+ newA.toList.sorted)
        System.err.println("goodA: "+goodA_left.toList.sorted)
      }else{
        System.err.println(" newS: "+ newS.toList.sorted)
        System.err.println("goodS: "+goodS_right.toList.sorted)
        
        System.err.println(" newA: "+ newA.toList.sorted)
        System.err.println("goodA: "+goodA_right.toList.sorted)
      }
      val wordAlignments = Monotonize.convertPhrasalToNormalAlignment(newS, newA)
      System.err.println("alignment: "+wordAlignments.map{case (i, j) => s"$i-$j"}.mkString(" "))
    }
  }

}
