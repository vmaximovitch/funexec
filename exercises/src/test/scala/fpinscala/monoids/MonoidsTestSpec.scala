/**
  */
package fpinscala.parsing

import fpinscala.state.RNG
import org.scalatest.Matchers
import org.scalatest.FlatSpec
import fpinscala.testing.Gen
import fpinscala.testing.Prop._
import fpinscala.monoids.Monoid

/** @author vladimirmaksimovich
  *
  */
class MonoidsTestSpec extends FlatSpec with Matchers {

  def fixture = new {
    val maxSize: Int = 100
    val testCases: Int = 100
    val rng: RNG = RNG.Simple(System.currentTimeMillis)
    val intGen = Gen.choose(-100, 100)
    val strGen = Gen.choose(0, 100).map(i => List.fill(i)(i.toString).mkString(" "))
  }

  "intAddition" should "satisfy 'monoids' laws" in {
    Monoid.monoidLaws(Monoid.intAddition, fixture.intGen).
      run(fixture.maxSize, fixture.testCases, fixture.rng) should (be(Passed) or be(Proved))
  }

  "intMultiplication" should "satisfy 'monoids' laws" in {
    Monoid.monoidLaws(Monoid.intMultiplication, fixture.intGen).
      run(fixture.maxSize, fixture.testCases, fixture.rng) should (be(Passed) or be(Proved))
  }

  "wcMonoid" should "satisfy 'monoids' laws" in {
    Monoid.monoidLaws(Monoid.wcMonoid, fixture.strGen.map(Monoid.Stub(_))).
      run(fixture.maxSize, fixture.testCases, fixture.rng) should (be(Passed) or be(Proved))
  }

}