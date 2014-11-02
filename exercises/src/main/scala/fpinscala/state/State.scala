package fpinscala.state

import scala.annotation.tailrec

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (rndInt, rngNext) = rng.nextInt
    (if (rndInt < 0) -(rndInt + 1) else rndInt, rngNext)
  }

  //  def double(rng: RNG): (Double, RNG) = {
  //    val (rndNonNegInt, rngNext) = RNG.nonNegativeInt(rng)
  //    (rndNonNegInt / (Int.MaxValue.toDouble + 1), rngNext)
  //  }

  def double: Rand[Double] = map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (rndNonNegInt, rngNext1) = RNG.nonNegativeInt(rng)
    val (rndDbl, rngNext2) = RNG.double(rngNext1)
    ((rndNonNegInt, rndDbl), rngNext2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((rndNonNegInt, rndDbl), rngNext1) = RNG.intDouble(rng)
    ((rndDbl, rndNonNegInt), rngNext1)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (rndDbl1, rngNext1) = RNG.double(rng)
    val (rndDbl2, rngNext2) = RNG.double(rngNext1)
    val (rndDbl3, rngNext3) = RNG.double(rngNext2)
    ((rndDbl1, rndDbl2, rndDbl3), rngNext3)
  }

  //  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
  //    @annotation.tailrec
  //    def go(acc: List[Int], rng: RNG, count: Int): (List[Int], RNG) =
  //      if (count <= 0) (acc, rng)
  //      else {
  //        val (rndInt, nextRng) = rng.nextInt
  //        go(rndInt :: acc, nextRng, count - 1)
  //      }
  //    go(Nil: List[Int], rng, count)
  //  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng1 => {
      val (a, rng2) = ra(rng1)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    // fold left and reverse the list via map
    map(fs.foldLeft(unit(List[A]()))((racc, ra) => map2(ra, racc)(_ :: _)))(_.reverse) 

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    val fs = List.fill(count)(int)
    sequence(fs)(rng)
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }
}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    sys.error("todo")
  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    sys.error("todo")
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    sys.error("todo")
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
