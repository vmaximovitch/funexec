/**
 */
package fpinscala.laziness

import org.scalatest._
import Stream._

/** @author vladimirmaksimovich
 *
 */
class StreamTestSpec extends FlatSpec with Matchers {
  def fixture = new {
    val sempty: Stream[Int] = Empty
    val s1 = Stream(1, 2, 3, 4, 5, 6)
  }

  "Stream.toList" should "return a list evaluated from the stream" in {
    val f = fixture
    info("toList of " + f.s1 + " -> " + f.s1.toList)
    f.s1.toList shouldBe List(1, 2, 3, 4, 5, 6)
  }

  "Stream.take" should "return first N elements of the stream" in {
    val f = fixture
    info("take 3 from " + f.s1 + " -> " + f.s1.take(3).toList)
    f.s1.take(3).toList shouldBe List(1, 2, 3)
    info("take 7 from " + f.s1 + " -> " + f.s1.take(7).toList)
    f.s1.take(7).toList shouldBe List(1, 2, 3, 4, 5, 6)
    info("take 3 from " + f.sempty + " -> " + f.sempty.take(3).toList)
    f.sempty.take(3).toList shouldBe Nil
  }

  "Stream.drop" should "return the stream without first N elements" in {
    val f = fixture
    info("drop 3 from " + f.s1 + " -> " + f.s1.drop(3).toList)
    f.s1.drop(3).toList shouldBe List(4, 5, 6)
    info("drop 7 from " + f.s1 + " -> " + f.s1.drop(7).toList)
    f.s1.drop(7).toList shouldBe List()
    info("take 3 from empty " + f.sempty + " -> " + f.sempty.drop(3).toList)
    f.sempty.drop(3).toList shouldBe Nil
  }

  "Stream.takeWhile" should "return all starting elements of a Stream that match the given predicate" in {
    val f = fixture
    info("take <3 from " + f.s1 + " -> " + f.s1.takeWhile(_ < 3).toList)
    f.s1.takeWhile(_ < 3).toList shouldBe List(1, 2)
    info("take <7 from " + f.s1 + " -> " + f.s1.takeWhile(_ < 7).toList)
    f.s1.takeWhile(_ < 7).toList shouldBe List(1, 2, 3, 4, 5, 6)
    info("take >6 from " + f.s1 + " -> " + f.s1.takeWhile(_ > 6).toList)
    f.s1.takeWhile(_ > 6).toList shouldBe List()
    info("take <3 from " + f.sempty + " -> " + f.sempty.takeWhile(_ < 3).toList)
    f.sempty.takeWhile(_ < 3).toList shouldBe Nil
  }
  
  "Stream.takeWhileFold" should "return all starting elements of a Stream that match the given predicate" in {
    val f = fixture
    info("take <3 from " + f.s1 + " -> " + f.s1.takeWhileFold(_ < 3).toList)
    f.s1.takeWhileFold(_ < 3).toList shouldBe List(1, 2)
    info("take <7 from " + f.s1 + " -> " + f.s1.takeWhileFold(_ < 7).toList)
    f.s1.takeWhileFold(_ < 7).toList shouldBe List(1, 2, 3, 4, 5, 6)
    info("take >6 from " + f.s1 + " -> " + f.s1.takeWhileFold(_ > 6).toList)
    f.s1.takeWhileFold(_ > 6).toList shouldBe List()
    info("take <3 from " + f.sempty + " -> " + f.sempty.takeWhileFold(_ < 3).toList)
    f.sempty.takeWhileFold(_ < 3).toList shouldBe Nil
  }

}