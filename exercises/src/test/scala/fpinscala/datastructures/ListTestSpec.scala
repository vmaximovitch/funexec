package fpinscala.datastructures

import org.scalatest._
import List._

/** @author vladimirmaksimovich
 *  tests for book - List
 */
class ListTestSpec extends FlatSpec with Matchers {
  "List.tail" should "return a tail of a list or Nil" in {
    val l = List(1, 2, 3, 4, 5)
    info("tail ->" + tail(l))
    tail(l) shouldBe List(2, 3, 4, 5)
    tail(List(1)) shouldBe Nil
  }

  "List.setHead" should "return a list wit new head or Nil" in {
    val l = List(1, 2, 3, 4, 5)
    info("setHead(9) ->" + setHead(l, 9))
    setHead(l, 9) shouldBe List(9, 2, 3, 4, 5)
  }

  "List.drop" should "return a list dropped n head elements or Nil" in {
    val l = List(1, 2, 3, 4, 5)
    info("drop(3) ->" + drop(l, 3))
    drop(l, 3) shouldBe List(4, 5)
    drop(Nil, 3) shouldBe Nil
    drop(l, 6) shouldBe Nil
  }

  "List.dropWhile" should "return a list dropped compliant head elements or Nil" in {
    val l = List(1, 2, 3, 4, 5)
    info("dropWhile(x < 3) ->" + dropWhile(l, (x: Int) => x < 3))
    dropWhile[Int](l, _ < 3) shouldBe List(3, 4, 5)
    dropWhile[Int](Nil, _ < 3) shouldBe Nil
    dropWhile[Int](l, _ < 6) shouldBe Nil
  }

  "List.init" should "return a list without last element or Nil" in {
    val l = List(1, 2, 3, 4, 5)
    info("init ->" + init(l))
    init(l) shouldBe List(1, 2, 3, 4)
    init(List(1)) shouldBe Nil
  }

  "List.length" should "return length of a list" in {
    val l = List(1, 2, 3, 4, 5)
    info("length ->" + List.length(l))
    List.length(l) shouldBe 5
    List.length(Nil) shouldBe 0
  }

  "List.reverse" should "return reversed list" in {
    val l = List(1, 2, 3, 4, 5)
    info("reverse of " + l + " ->" + List.reverse(l))
    List.reverse(l) should be(List(5, 4, 3, 2, 1))
  }

  "List.append2" should "return 2 appended lists into one" in {
    val l1 = List(1, 2, 3)
    val l2 = List(6, 7, 8)
    info("append of " + l1 + " and " + l2 + " ->" + List.append2(l1, l2))
    List.append2(l1, l2) shouldBe List.append(l1, l2)
  }

  "List.concat" should "return flatten list" in {
    val l = List(List(1, 2), List(3, 4, 5), List(6, 7, 8), List())
    info("concat of " + l + " ->" + List.concat(l))
    List.concat(l) shouldBe List(1, 2, 3, 4, 5, 6, 7, 8)
  }

  "List.map" should "return mapped list" in {
    val l = List(1, 2, 3, 4, 5)
    info("map (x*2) of " + l + " ->" + List.map(l)(_ * 2))
    List.map(l)(_ * 2) shouldBe List(2, 4, 6, 8, 10)
  }

  "List.filter" should "return filtered list" in {
    val l = List(1, 2, 3, 4, 5)
    info("filter (x%2 == 0) on " + l + " ->" + List.filter(l)(_ % 2 == 0))
    List.filter(l)(_ % 2 == 0) shouldBe List(1, 3, 5)
  }

  "List.flatMap" should "return flatten mapped list" in {
    val l = List(1, 2, 3, 4, 5)
    info("flatMap (x*2, x*3) of " + l + " ->" + List.flatMap(l)(x => List(x * 2, x * 3)))
    List.flatMap(l)(x => List(x * 2, x * 3)) shouldBe List(2, 3, 4, 6, 6, 9, 8, 12, 10, 15)
  }

  "List.zipWith" should "return zipped list" in {
    val l1 = List(1, 2, 3, 4)
    val l2 = List(6, 7, 8)
    info("zip (a*b) of " + l1 + " and " + l2 + " ->" + List.zipWith(l1, l2)(_ * _))
    List.zipWith(l1, l2)(_ * _) shouldBe List(6, 14, 24)
  }

  "List.hasSubsequence" should "return true if there is a subsequence" in {
    val l1 = List(1, 2, 3, 4)
    val sub1 = List(2, 3, 4, 5)
    val sub2 = List(3, 4)
    info("hasSubsequence of " + sub1 + " in " + l1 + " ->" + List.hasSubsequence(l1, sub1))
    List.hasSubsequence(l1, sub1) shouldBe false
    info("hasSubsequence of " + sub2 + " in " + l1 + " ->" + List.hasSubsequence(l1, sub2))
    List.hasSubsequence(l1, sub2) shouldBe true
  }

  //  "List.product" should "multiply elements of a list" in {
  //    val l = List[Double](1, 2, 3, 4, 5)
  //    info("product ->" + product(l))
  //    product(l) should be(120.0)
  //  }
  //  "List.appendViaFoldLeft" should "append two lists" in {
  //    val l1 = List(1,2,3,4,5)
  //    val l2 = List(6,7,8,9)
  //    info("append of " + l1 + "+" + l2 +" -> " + List.appendViaFoldLeft(l1,l2))
  //    List.appendViaFoldLeft(l1, l2) should be (List(1,2,3,4,5,6,7,8,9))
  //  }
  //  "List.concat" should "append two lists" in {
  //    val ll = List( List(1,2,3), List(4,5,6), List(7,8,9))
  //    info("concat of " + ll + " -> " + List.concat(ll))
  //    List.concat(ll) should be (List(1,2,3,4,5,6,7,8,9))
  //  }
}