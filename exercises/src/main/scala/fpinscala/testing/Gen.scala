package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{ Executors, ExecutorService }

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop {
    (ms, tc, rnd) =>
      this.run(ms, tc, rnd) match {
        case Passed | Proved => p.run(ms, tc, rnd)
        case p => p
      }
    // def check = Prop.this.check && p.check
  }
  def ||(p: Prop): Prop = Prop {
    (ms, tc, rnd) =>
      this.run(ms, tc, rnd) match {
        case Falsified(msg, count) => p.run(ms, tc, rnd) match {
          // need to inject msg and count into the results
          case Falsified(msg1, count1) => Falsified(msg + "\n" + msg1, count + count1)
          case s => s
        }
        case p => p
      }
    // def check = Prop.this.check || p.check
  }
}

object Prop {
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int
  type FailedCase = String

  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    def isFalsified = false
  }
  case class Falsified(failure: FailedCase,
    successes: SuccessCount) extends Result {
    def isFalsified = true
  }
  case object Proved extends Result {
    def isFalsified = false
  }

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = Prop {
    (max, tc, rng) =>
      {
        randomStream(gen)(rng).zip(Stream.from(0)).take(max).map {
          case (a, i) => try {
            if (f(a)) Passed else Falsified(a.toString, i)
          } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
        }.find(_.isFalsified).getOrElse(Passed)
      }
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      {
        val casesPerSize = (n + (max - 1)) / max
        val props: Stream[Prop] =
          Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
        val prop: Prop =
          props.map(p => Prop { (max, _, rng) =>
            p.run(max, casesPerSize, rng)
          }).toList.reduce(_ && _)
        prop.run(max, n, rng)
      }
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))
  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  // run property
  def run(p: Prop,
    maxSize: Int = 100,
    testCases: Int = 100,
    rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }

  // check condition of the property
  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Proved else Falsified("()", 0)
  }

}

object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt) map (n => start + n % (stopExclusive - start)))
  def boolean: Gen[Boolean] =
    Gen(State(RNG.int) map (n => if (n % 2 == 0) true else false))
  def int: Gen[Int] = Gen(State(RNG.int))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(g.listOfN(_))
  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(n => g.listOfN(n max 1))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean flatMap (b => if (b) g1 else g2)
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    /* The probability we should pull from `g1`. */
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)
    Gen(State(RNG.double).flatMap(d => if (d < g1Threshold) g1._1.sample else g2._1.sample))
  }

  // unapply a tuple 
  object ** {
    def unapply[A, B](p: (A, B)) = Some(p)
  }

  // generate 
  def genStringAFn[A](g: Gen[A]): Gen[String => A] = Gen {
    State { (rng: RNG) =>
      {
        val (seed, rng2) = rng.nextInt // we still use `rng` to produce a seed, so we get a new function each time
        val f = (s: String) => g.sample.run(RNG.Simple(seed.toLong ^ s.hashCode.toLong))._1
        (f, rng2)
      }
    }
  }
}

case class Gen[+A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] = Gen(sample map f)
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(f(_).sample))
  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] = Gen(sample.map2(g.sample)(f))

  def listOfN(size: Gen[Int]): Gen[List[A]] = size flatMap (n => Gen.listOfN(n, this))
  def listOfN(n: Int): Gen[List[A]] = Gen.listOfN(n, this)
  def unsized: SGen[A] = SGen(_ => this)

  // product of 2 generators
  def **[B](g: Gen[B]): Gen[(A, B)] =
    (this map2 g)((_, _))
}

case class SGen[+A](forSize: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = forSize(n)
  def map[B](f: A => B): SGen[B] = SGen(forSize andThen (_ map f))
  def flatMap[B](f: A => SGen[B]): SGen[B] = SGen(n => forSize(n).flatMap(a => f(a).forSize(n)))
  def map2[B, C](g: SGen[B])(f: (A, B) => C): SGen[C] = for {
    a <- this
    b <- g
  } yield f(a, b)
}

