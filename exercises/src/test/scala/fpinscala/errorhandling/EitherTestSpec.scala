package fpinscala.errorhandling

import org.scalatest._
import Either._

class EitherTestSpec extends FlatSpec with Matchers {
  def fixture = new {
    val e = new Error()
    val e1 = new Error("error1")
    val e2 = new Error("error2")
  }

  "Either.map" should "return mapped right value or left" in {
    val f = fixture
    val r: Either[Error, Int] = Right(1)
    val l: Either[Error, Int] = Left(f.e)
    info("map of " + r + " to +2 -> " + r.map(_ + 2))
    r.map(_ + 2) shouldBe Right(3)
    info("map of " + l + " to +2 -> " + l.map(_ + 2))
    l.map(_ + 2) shouldBe Left(f.e)
  }

  "Either.flatMap" should "return Either with mapped right value or left" in {
    val f = fixture
    val r: Either[Error, Int] = Right(1)
    val l: Either[Error, Int] = Left(f.e)
    info("flatMap of " + r + " to Right(+2) -> " + r.flatMap(x => Right(x + 2)))
    r.flatMap(x => Right(x + 2)) shouldBe Right(3)
    info("flatMap of " + l + " to Right(+2) -> " + l.flatMap(x => Right(x + 2)))
    l.flatMap(x => Right(x + 2)) shouldBe Left(f.e)
  }

  "Either.orElse" should "return Either or default" in {
    val f = fixture
    val d = Right(15)
    val r: Either[Error, Int] = Right(1)
    val l: Either[Error, Int] = Left(f.e)
    info("orElse of " + r + " with " + d + " -> " + r.orElse(d))
    r.orElse(d) shouldBe Right(1)
    info("orElse of " + l + " with " + d + " -> " + l.orElse(d))
    l.orElse(d) shouldBe d
  }

  "Either.map2" should "return Either or default" in {
    val f = fixture
    val r1: Either[Error, Int] = Right(2)
    val r2: Either[Error, Int] = Right(3)
    val l: Either[Error, Int] = Left(f.e)
    info("map2( " + r1 + ", " + r2 + " ) using x*y -> " + r1.map2(r2)(_ * _))
    r1.map2(r2)(_ * _) shouldBe Right(2 * 3)
    info("map2( " + l + ", " + r1 + " ) using x*y -> " + l.map2(r1)(_ * _))
    l.map2(r1)(_ * _) shouldBe Left(f.e)
    info("map2( " + r2 + ", " + l + " ) using x*y -> " + r2.map2(l)(_ * _))
    r2.map2(l)(_ * _) shouldBe Left(f.e)
  }

  "Either.sequence" should "return Either of a list" in {
    val f = fixture
    val lr = List(Right(1), Right(2), Right(3), Right(4))
    val ll = List(Right(1), Left(f.e), Right(3), Right(4))
    info("sequence of " + lr + "  -> " + sequence(lr))
    sequence(lr) shouldBe Right(List(1, 2, 3, 4))
    info("sequence of " + ll + "  -> " + sequence(ll))
    sequence(ll) shouldBe Left(f.e)
  }

  "Option.traverse" should "return Either of a list mapped by a function" in {
    val f = fixture
    val l = List(1, 2, 3, 4)
    info("traverse of " + l + " with Right(*2) -> " + traverse(l)(x => Right(x * 2)))
    traverse(l)(x => Right(x * 2)) shouldBe Right(List(2, 4, 6, 8))
    info("traverse of " + l + " with Right(*2) and Left(e) -> " + traverse(l)(x => if (x != 2) Right(x * 2) else Left(f.e)))
    traverse(l)(x => if (x != 2) Right(x * 2) else Left(f.e)) shouldBe Left(f.e)
  }

  "Either.map2L" should "return Either or default" in {
    val f = fixture
    val r1: Either[List[Error], Int] = Right(2)
    val r2: Either[List[Error], Int] = Right(3)
    val l1: Either[List[Error], Int] = Left(List(f.e1))
    val l2: Either[List[Error], Int] = Left(List(f.e2))
    info("map2L( " + r1 + ", " + r2 + " ) using x*y -> " + map2L(r1, r2)(_ * _))
    map2L(r1, r2)(_ * _) shouldBe Right(2 * 3)
    info("map2L( " + l1 + ", " + l2 + " ) using x*y -> " + map2L(l1, l2)(_ * _))
    map2L(l1, l2)(_ * _) shouldBe Left(List(f.e1, f.e2))
  }

  "Either.sequenceL" should "return Either of a list" in {
    val f = fixture
    val lr = List(Right(1), Right(2), Right(3), Right(4))
    val ll = List(Right(1), Left(List(f.e)), Right(3), Left(List(f.e)))
    info("sequence of " + lr + "  -> " + sequenceL(lr))
    sequenceL(lr) shouldBe Right(List(1, 2, 3, 4))
    info("sequence of " + ll + "  -> " + sequenceL(ll))
    sequenceL(ll) shouldBe Left(List(f.e, f.e))
  }

  "Option.traverseL" should "return Either of a list mapped by a function" in {
    val f = fixture
    val l = List(1, 2, 3, 4)
    info("traverse of " + l + " with Right(*2) -> " + traverseL(l)(x => Right(x * 2)))
    traverseL(l)(x => Right(x * 2)) shouldBe Right(List(2, 4, 6, 8))
    info("traverse of " + l + " with Right(*2) and Left(e) -> " +
      traverseL(l)(x => if (x%2 != 0) Right(x * 2) else Left(List(f.e))))
    traverseL(l)(x => if (x%2 != 0) Right(x * 2) else Left(List(f.e))) shouldBe Left(List(f.e, f.e))
  }
}