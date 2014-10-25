/**
 * 
 */
package fpinscala.datastructures

import org.scalatest._

/**
 * @author vladimirmaksimovich
 *
 */
class TestTestSpec extends FlatSpec with Matchers {
  
  def fixture =
    new {
      val ti: Tree[Int] = Branch(
    		  				Branch(Leaf(1), Leaf(2)),
    		  				Branch(Leaf(3),
    		  						Branch(Leaf(4), Leaf(5))))
    }
  
  "Tree" should "be able to calculate size" in {
    val f = fixture
    info("tree: ->" + f.ti)
    Tree.size(f.ti) should be (9)
  }
  
  it should "be able to calculate maxmum element" in {
    val f = fixture
    info("tree: ->" + f.ti)
    Tree.maximum(f.ti) should be (5)
  }

}