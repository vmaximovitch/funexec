/**
 */
package fpinscala.state

import org.scalatest._
import Stream._

/** @author vladimirmaksimovich
 *
 */
class StateTestSpec extends FlatSpec with Matchers {
  def fixture = new {
  }

  "State.toList" should "return a list evaluated from the stream" in {
    val f = fixture
    // info("toList of " + f.s1.toList + " -> " + f.s1.toList)
    // f.s1.toList shouldBe List(1, 2, 3, 4, 5, 6)
  }

}