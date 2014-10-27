package fpinscala.errorhandling

import org.scalatest._
import Option._

class OptionTestSpec extends FlatSpec with Matchers {
  "Option.map" should "return option with mapped value or None" in {
    val o = Some(1)
    val onone: Option[Int] = None
    info("map of " + o + " to +2 -> " + o.map(_ + 2))
    o.map(_ + 2) shouldBe Some(3)
    onone.map(_ + 2) shouldBe None
  }

  "Option.flatMap" should "return option with mapped value or None" in {
    val o = Some(1)
    val onone: Option[Int] = None
    info("flatMap of " + o + " to Some(+2) -> " + o.flatMap(x => Some(x + 2)))
    o.flatMap(x => Some(x + 2)) shouldBe Some(3)
    onone.flatMap(x => Some(x + 2)) shouldBe None
  }

  "Option.getOrElse" should "return option value or default" in {
    val o = Some(1)
    val onone: Option[Int] = None
    info("getOrElse of " + o + " -> " + o.getOrElse(2))
    o.getOrElse(2) shouldBe 1
    onone.getOrElse(2) shouldBe 2
  }

  "Option.orElse" should "return option or default" in {
    val o = Some(1)
    val onone: Option[Int] = None
    info("orElse of " + o + " -> " + o.orElse(Some(2)))
    o.orElse(Some(2)) shouldBe Some(1)
    onone.orElse(Some(2)) shouldBe Some(2)
  }

  "Option.filter" should "return values if predicate is positive" in {
    val o = Some(1)
    val onone: Option[Int] = None
    info("filter of " + o + " with (%2 != 0) -> " + o.filter(_ % 2 != 0))
    o.filter(_ % 2 == 0) shouldBe None
    o.filter(_ % 2 != 0) shouldBe Some(1)
    onone.filter(_ % 2 == 0) shouldBe None
  }

  "Option.variance" should "return variance of a sequence or None" in {
    val seq = Seq(1.0, 2.0, 3.0, 4.0)
    info("mean of " + seq + "  -> " + mean(seq))
    info("variance of " + seq + "  -> " + variance(seq))
    variance(seq) shouldBe Some(1.25D)
    variance(Nil) shouldBe None
  }

  "Option.sequence" should "return option of a list" in {
    val l = List(Some(1), Some(2), Some(3), Some(4))
    val ln = List(Some(1), None, Some(3), Some(4))
    info("sequence of " + l + "  -> " + sequence(l))
    sequence(l) shouldBe Some(List(1, 2, 3, 4))
    info("sequence of " + ln + "  -> " + sequence(ln))
    sequence(ln) shouldBe None
  }

  "Option.traverse" should "return optio of a list mapped by a function" in {
    val l = List(1, 2, 3, 4)
    info("traverse of " + l + " with *2 -> " + traverse(l)(x => Some(x * 2)))
    traverse(l)(x => Some(x * 2)) shouldBe Some(List(2, 4, 6, 8))
    info("traverse of " + l + " with *2 and None -> " + traverse(l)(x => if(x != 2) Some(x * 2) else None))
    traverse(l)(x => if(x != 2) Some(x * 2) else None) shouldBe None
  }
}