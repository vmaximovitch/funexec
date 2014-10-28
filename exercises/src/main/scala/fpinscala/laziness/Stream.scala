package fpinscala.laziness

import Stream._
trait Stream[+A] {

  // evalua tream and convert to List
  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Empty => acc
      case Cons(h, t) => go(t(), h() :: acc)
    }
    go(this, List()).reverse
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  // return the first n elements of a Stream
  // @annotation.tailrec
  def take(n: Int): Stream[A] =
    if (n <= 0) Empty
    else this match {
      case Empty => this
      case Cons(h, t) => cons(h(), t().take(n - 1))
    }

  // return all starting elements of a Stream that match the given predicate
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => this
    case Cons(h, t) => {
      lazy val hd = h()
      if (p(hd)) cons(hd, t().takeWhile(p))
      else Empty
    }
  }

  // returning all starting elements of a Stream that match the given predicate
  def takeWhileFold(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty)

  // skip the first n elements of a Stream
  def drop(n: Int): Stream[A] = {
    @annotation.tailrec
    def go(s: Stream[A], count: Int): Stream[A] =
      if (count <= 0) s
      else s match {
        case Empty => s
        case Cons(_, t) => go(t(), count - 1)
      }
    go(this, n)
  }

  // checks that all elements in the Stream match a given predicate
  def forAll(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) && b)

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")

  def headOption: Option[A] = foldRight(None: Option[A])((a, _) => Some(a))

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  def flatMap[B >: A](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => f(a) append b)

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((a, b) => cons(a, b))
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  // stream of constants
  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  // incremental steam of integers
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  // Fibonacci numbers
  def fib(): Stream[Int] = {
    def fibGo(prev: Int, n: Int): Stream[Int] = cons(n, fibGo(n, n + prev))
    fibGo(0, 1)
  }

  // Fibonacci numbers
  def fibUnfold(): Stream[Int] =
    unfold((0, 1)) { case (a, s) => Some((a, (s, s + a))) }
  // incremental steam of integers
  def fromUnfold(n: Int): Stream[Int] =
    unfold(n)(next => Some(next, next + 1))
  val onesUnfold: Stream[Int] = unfold(1)(_ => Some(1, 1))
  def constantUnfold[A](a: A): Stream[A] = unfold(a)(_ => Some(a, a))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case _ => empty
  }
}
