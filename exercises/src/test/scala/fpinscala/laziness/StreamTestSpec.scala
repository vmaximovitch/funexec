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
    val s1sub = Stream(1, 2, 3)
    val s2 = Stream(11, 12, 13)
    val s2sub = Stream(12, 13)
    val constS = "abc"
  }

  "Stream.toList" should "return a list evaluated from the stream" in {
    val f = fixture
    info("toList of " + f.s1.toList + " -> " + f.s1.toList)
    f.s1.toList shouldBe List(1, 2, 3, 4, 5, 6)
  }

  "Stream.take" should "return first N elements of the stream" in {
    val f = fixture
    info("take 3 from " + f.s1.toList + " -> " + f.s1.take(3).toList)
    f.s1.take(3).toList shouldBe List(1, 2, 3)
    info("take 7 from " + f.s1.toList + " -> " + f.s1.take(7).toList)
    f.s1.take(7).toList shouldBe List(1, 2, 3, 4, 5, 6)
    info("take 3 from " + f.sempty.toList + " -> " + f.sempty.take(3).toList)
    f.sempty.take(3).toList shouldBe Nil
  }

  "Stream.takeUnfold" should "return first N elements of the stream" in {
    val f = fixture
    info("take 3 from " + f.s1.toList + " -> " + f.s1.takeUnfold(3).toList)
    f.s1.takeUnfold(3).toList shouldBe List(1, 2, 3)
    info("take 7 from " + f.s1.toList + " -> " + f.s1.takeUnfold(7).toList)
    f.s1.takeUnfold(7).toList shouldBe List(1, 2, 3, 4, 5, 6)
    info("take 3 from " + f.sempty.toList + " -> " + f.sempty.takeUnfold(3).toList)
    f.sempty.takeUnfold(3).toList shouldBe Nil
  }

  "Stream.drop" should "return the stream without first N elements" in {
    val f = fixture
    info("drop 3 from " + f.s1.toList + " -> " + f.s1.drop(3).toList)
    f.s1.drop(3).toList shouldBe List(4, 5, 6)
    info("drop 7 from " + f.s1.toList + " -> " + f.s1.drop(7).toList)
    f.s1.drop(7).toList shouldBe List()
    info("take 3 from empty " + f.sempty.toList + " -> " + f.sempty.drop(3).toList)
    f.sempty.drop(3).toList shouldBe Nil
  }

  "Stream.takeWhile" should "return all starting elements of a Stream that match the given predicate" in {
    val f = fixture
    info("take <3 from " + f.s1.toList + " -> " + f.s1.takeWhile(_ < 3).toList)
    f.s1.takeWhile(_ < 3).toList shouldBe List(1, 2)
    info("take <7 from " + f.s1.toList + " -> " + f.s1.takeWhile(_ < 7).toList)
    f.s1.takeWhile(_ < 7).toList shouldBe List(1, 2, 3, 4, 5, 6)
    info("take >6 from " + f.s1.toList + " -> " + f.s1.takeWhile(_ > 6).toList)
    f.s1.takeWhile(_ > 6).toList shouldBe List()
    info("take <3 from " + f.sempty.toList + " -> " + f.sempty.takeWhile(_ < 3).toList)
    f.sempty.takeWhile(_ < 3).toList shouldBe Nil
  }

  "Stream.takeWhileFold" should "return all starting elements of a Stream that match the given predicate" in {
    val f = fixture
    info("take <3 from " + f.s1.toList + " -> " + f.s1.takeWhileFold(_ < 3).toList)
    f.s1.takeWhileFold(_ < 3).toList shouldBe List(1, 2)
    info("take <7 from " + f.s1.toList + " -> " + f.s1.takeWhileFold(_ < 7).toList)
    f.s1.takeWhileFold(_ < 7).toList.toList shouldBe List(1, 2, 3, 4, 5, 6)
    info("take >6 from " + f.s1.toList + " -> " + f.s1.takeWhileFold(_ > 6).toList)
    f.s1.takeWhileFold(_ > 6).toList shouldBe List()
    info("take <3 from " + f.sempty.toList + " -> " + f.sempty.takeWhileFold(_ < 3).toList)
    f.sempty.takeWhileFold(_ < 3).toList shouldBe Nil
  }

  "Stream.takeWhileUnfold" should "return all starting elements of a Stream that match the given predicate" in {
    val f = fixture
    info("take <3 from " + f.s1.toList + " -> " + f.s1.takeWhileUnfold(_ < 3).toList)
    f.s1.takeWhileUnfold(_ < 3).toList shouldBe List(1, 2)
    info("take <7 from " + f.s1.toList + " -> " + f.s1.takeWhileUnfold(_ < 7).toList)
    f.s1.takeWhileUnfold(_ < 7).toList.toList shouldBe List(1, 2, 3, 4, 5, 6)
    info("take >6 from " + f.s1.toList + " -> " + f.s1.takeWhileUnfold(_ > 6).toList)
    f.s1.takeWhileUnfold(_ > 6).toList shouldBe List()
    info("take <3 from " + f.sempty.toList + " -> " + f.sempty.takeWhileUnfold(_ < 3).toList)
    f.sempty.takeWhileUnfold(_ < 3).toList shouldBe Nil
  }

  "Stream.forAll" should "return true if all elements in the Stream match a given predicate" in {
    val f = fixture
    info("forAll (>= 1) in " + f.s1.toList + " -> " + f.s1.forAll(_ >= 1))
    f.s1.forAll(_ >= 1) shouldBe true
    info("forAll (< 4) in " + f.s1.toList + " -> " + f.s1.forAll(_ < 4))
    f.s1.forAll(_ < 4) shouldBe false
    info("forAll (== 0) in " + f.sempty.toList + " -> " + f.sempty.forAll(_ == 0))
    f.s1.forAll(_ == 0) shouldBe false
  }

  "Stream.headOption" should "return Some with head value or None" in {
    val f = fixture
    info("headOption for " + f.s1.toList + " -> " + f.s1.headOption)
    f.s1.headOption shouldBe Some(1)
    info("headOption for " + f.sempty.toList + " -> " + f.sempty.headOption)
    f.sempty.headOption shouldBe None
  }

  "Stream.map" should "return stream mapped by function" in {
    val f = fixture
    info("map (x*2) of " + f.s1.toList + " ->" + f.s1.map(_ * 2).toList)
    f.s1.map(_ * 2).toList shouldBe List(2, 4, 6, 8, 10, 12)
    f.sempty.map(_ * 2).toList shouldBe Nil
  }

  "Stream.mapUnfold" should "return stream mapped by function" in {
    val f = fixture
    info("mapUnfold (x*2) of " + f.s1.toList + " ->" + f.s1.mapUnfold(_ * 2).toList)
    f.s1.mapUnfold(_ * 2).toList shouldBe List(2, 4, 6, 8, 10, 12)
    f.sempty.mapUnfold(_ * 2).toList shouldBe Nil
  }

  "Stream.filter" should "return filtered stream" in {
    val f = fixture
    info("filter (x%2 == 0) on " + f.s1.toList + " ->" + f.s1.filter(_ % 2 == 0).toList)
    f.s1.filter(_ % 2 == 0).toList shouldBe List(2, 4, 6)
    f.sempty.filter(_ % 2 == 0).toList shouldBe Nil
  }

  "Stream.flatMap" should "return flatten mapped stream" in {
    val f = fixture
    info("flatMap (x*2, x*3) of " + f.s1.toList + " ->" + f.s1.flatMap(x => Stream(x * 2, x * 3)).toList)
    f.s1.flatMap(x => Stream(x * 2, x * 3)).toList shouldBe List(2, 3, 4, 6, 6, 9, 8, 12, 10, 15, 12, 18)
    f.sempty.flatMap(x => Stream(x * 2, x * 3)).toList shouldBe Nil
  }

  "Stream.append" should "return 2 appended streams into one" in {
    val f = fixture
    info("append of " + f.s1.toList + " and " + f.s2.toList + " ->" + f.s1.append(f.s2).toList)
    f.s1.append(f.s2).toList shouldBe f.s1.toList ++ f.s2.toList
    f.sempty.append(f.s2).toList shouldBe f.s2.toList
    f.s1.append(f.sempty).toList shouldBe f.s1.toList
  }

  "Stream.constant" should "return infinite constant stream" in {
    val f = fixture
    info("constant stream of \"" + f.constS + "\" limited by 5 ->" + Stream.constant(f.constS).take(5).toList)
    Stream.constant(f.constS).take(5).toList shouldBe List(f.constS, f.constS, f.constS, f.constS, f.constS)
  }

  "Stream.constantUnfold" should "return infinite constant stream" in {
    val f = fixture
    info("constant stream of \"" + f.constS + "\" limited by 5 ->" + Stream.constantUnfold(f.constS).take(5).toList)
    Stream.constantUnfold(f.constS).take(5).toList shouldBe List(f.constS, f.constS, f.constS, f.constS, f.constS)
  }

  "Stream.from" should "return infinite stream of incrementing numbers" in {
    val f = fixture
    info("Incrementing numbers starting from 6 limited by 5 ->" + Stream.from(6).take(5).toList)
    Stream.from(6).take(5).toList shouldBe List(6, 7, 8, 9, 10)
  }

  "Stream.fromUnfold" should "return infinite stream of incrementing numbers" in {
    val f = fixture
    info("Incrementing numbers starting from 6 limited by 5 ->" + Stream.fromUnfold(6).take(5).toList)
    Stream.fromUnfold(6).take(5).toList shouldBe List(6, 7, 8, 9, 10)
  }

  "Stream.fibs" should "return infinite stream of Fibonacci numbers" in {
    val f = fixture
    info("Fibonacci stream starting limited by 7 ->" + Stream.fibs.take(8).toList)
    Stream.fibs.take(8).toList shouldBe List(0, 1, 1, 2, 3, 5, 8, 13)
  }

  "Stream.fibsUnfold" should "return infinite stream of Fibonacci numbers" in {
    val f = fixture
    info("Fibonacci stream starting limited by 7 ->" + Stream.fibsUnfold.take(8).toList)
    Stream.fibsUnfold.take(8).toList shouldBe List(0, 1, 1, 2, 3, 5, 8, 13)
  }

  "Stream.zipWith" should "return zipped list" in {
    val f = fixture
    info("zip (a*b) of " + f.s1.toList + " and " + f.s2.toList + " ->" + f.s1.zipWith(f.s2)(_ * _).toList)
    f.s1.zipWith(f.s2)(_ * _).toList shouldBe List(11, 24, 39) // val s1 = Stream(1, 2, 3, 4, 5, 6) val s2 = Stream(11, 12, 13)
    info("zip (a*b) of " + f.sempty.toList + " and " + f.s2.toList + " ->" + f.sempty.zipWith(f.s2)(_ * _).toList)
    f.sempty.zipWith(f.s2)(_ * _).toList shouldBe Nil
    info("zip (a*b) of " + f.s1.toList + " and " + f.sempty.toList + " ->" + f.s1.zipWith(f.sempty)(_ * _).toList)
    f.s1.zipWith(f.sempty)(_ * _).toList shouldBe Nil
  }

  "Stream.zipAll" should "return zipped list" in {
    val f = fixture
    info("zip all of " + f.s1.toList + " and " + f.s2.toList + " ->" + f.s1.zipAll(f.s2).toList)
    f.s1.zipAll(f.s2).toList shouldBe List((Some(1), Some(11)), (Some(2), Some(12)), (Some(3), Some(13)),
      (Some(4), None), (Some(5), None), (Some(6), None)) // val s1 = Stream(1, 2, 3, 4, 5, 6) val s2 = Stream(11, 12, 13)
    info("zip all of " + f.sempty.toList + " and " + f.s2.toList + " ->" + f.sempty.zipAll(f.s2).toList)
    f.sempty.zipAll(f.s2).toList shouldBe List((None, Some(11)), (None, Some(12)), (None, Some(13)))
    info("zip all of " + f.s1.toList + " and " + f.sempty.toList + " ->" + f.s1.zipAll(f.sempty).toList)
    f.s1.zipAll(f.sempty).toList shouldBe List((Some(1), None), (Some(2), None), (Some(3), None), (Some(4), None), (Some(5), None), (Some(6), None))
  }

  "Stream.zipWithAll" should "return function zipped list" in {
    val f = fixture
    info("zip all using == of " + f.s1.toList + " and " + f.s2.toList + " ->" + f.s1.zipWithAll(f.s2)(_ == _).toList)
    f.s1.zipWithAll(f.s2)(_ == _).toList shouldBe List(false, false, false, false, false, false) // s1 = Stream(1, 2, 3, 4, 5, 6) s2 = Stream(11, 12, 13)
    info("zip all using == of " + f.s1.toList + " and " + f.s1sub.toList + " ->" + f.s1.zipWithAll(f.s1sub)(_ == _).toList)
    f.s1.zipWithAll(f.s1sub)(_ == _).toList shouldBe List(true, true, true, false, false, false) // s1 = Stream(1, 2, 3, 4, 5, 6) s1sub = Stream(1, 2, 3)
    info("zip all using ==  of " + f.sempty.toList + " and " + f.s2.toList + " ->" + f.sempty.zipWithAll(f.s2)(_ == _).toList)
    f.sempty.zipWithAll(f.s2)(_ == _).toList shouldBe List(false, false, false)
    info("zip all using == of " + f.s1.toList + " and " + f.sempty.toList + " ->" + f.s1.zipWithAll(f.sempty)(_ == _).toList)
    f.s1.zipWithAll(f.sempty)(_ == _).toList shouldBe List(false, false, false, false, false, false)
  }

  "Stream.startsWith" should "return true if stream starts with other stream" in {
    val f = fixture
    info("stream " + f.s1.toList + " starts with " + f.s1sub.toList + " ->" + f.s1.startsWith(f.s1sub))
    f.s1.startsWith(f.s1sub) shouldBe true
    info("stream " + f.s1sub.toList + " starts with " + f.s1.toList + " ->" + f.s1sub.startsWith(f.s1))
    f.s1sub.startsWith(f.s1) shouldBe false
    info("stream " + f.s1.toList + " starts with " + f.s2.toList + " ->" + f.s1.startsWith(f.s2))
    f.s1.startsWith(f.s2) shouldBe false
    info("stream " + f.s1.toList + " starts with " + f.sempty.toList + " ->" + f.s1.startsWith(f.sempty))
    f.s1.startsWith(f.sempty) shouldBe true
    info("stream " + f.sempty.toList + " starts with " + f.s2.toList + " ->" + f.sempty.startsWith(f.s2))
    f.sempty.startsWith(f.s2) shouldBe false
    info("stream " + f.sempty.toList + " starts with " + f.sempty.toList + " ->" + f.sempty.startsWith(f.sempty))
    f.sempty.startsWith(f.sempty) shouldBe true
  }

  "Stream.tails" should "return all tail streams" in {
    val f = fixture
    info("tails of " + f.s2.toList + " ->" + f.s2.tails.toList.map(_.toList))
    f.s2.tails.toList.map(_.toList) shouldBe List(List(11, 12, 13), List(12, 13), List(13), List())
    info("tails of " + f.sempty.toList + " ->" + f.sempty.tails.toList.map(_.toList))
    f.sempty.tails.toList.map(_.toList) shouldBe List(List())
  }

  "Stream.hasSubsequence" should "return true if stream includes other stream" in {
    val f = fixture
    info("stream " + f.s1.toList + " hasSubsequence of " + f.s1sub.toList + " ->" + f.s1.hasSubsequence(f.s1sub))
    f.s1.hasSubsequence(f.s1sub) shouldBe true
    info("stream " + f.s1sub.toList + " hasSubsequence of " + f.s1.toList + " ->" + f.s1sub.hasSubsequence(f.s1))
    f.s1sub.hasSubsequence(f.s1) shouldBe false
    info("stream " + f.s2.toList + " hasSubsequence of " + f.s2sub.toList + " ->" + f.s2.hasSubsequence(f.s2sub))
    f.s2.hasSubsequence(f.s2sub) shouldBe true
    info("stream " + f.s2sub.toList + " hasSubsequence of " + f.s2.toList + " ->" + f.s2sub.hasSubsequence(f.s2))
    f.s2sub.hasSubsequence(f.s2) shouldBe false
    info("stream " + f.s1.toList + " hasSubsequence of " + f.s2.toList + " ->" + f.s1.hasSubsequence(f.s2))
    f.s1.hasSubsequence(f.s2) shouldBe false
    info("stream " + f.s1.toList + " hasSubsequence of " + f.sempty.toList + " ->" + f.s1.hasSubsequence(f.sempty))
    f.s1.hasSubsequence(f.sempty) shouldBe true
    info("stream " + f.sempty.toList + " hasSubsequence of " + f.s2.toList + " ->" + f.sempty.hasSubsequence(f.s2))
    f.sempty.hasSubsequence(f.s2) shouldBe false
    info("stream " + f.sempty.toList + " hasSubsequence of " + f.sempty.toList + " ->" + f.sempty.hasSubsequence(f.sempty))
    f.sempty.hasSubsequence(f.sempty) shouldBe true
  }

  "Stream.scanRight" should "return all tail streams" in {
    val f = fixture
    info("tails of " + f.s2.toList + " ->" + f.s2.tails.toList.map(_.toList))
    f.s2.tails.toList.map(_.toList) shouldBe List(List(11, 12, 13), List(12, 13), List(13), List())
    info("tails of " + f.sempty.toList + " ->" + f.sempty.tails.toList.map(_.toList))
    f.sempty.tails.toList.map(_.toList) shouldBe List(List())
  }

}