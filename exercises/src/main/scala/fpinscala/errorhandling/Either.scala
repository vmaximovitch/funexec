package fpinscala.errorhandling

import scala.{ Option => _, Either => _, Left => _, Right => _, _ } // hide std library `Option` and `Either`, since we are writing our own in this chapter

// custom Either
sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case e: Left[_] => e
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case e: Left[_] => e
    case Right(a) => f(a)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case e: Left[_] => b
    case a: Right[_] => a
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      aa <- this
      bb <- b
    } yield f(aa, bb)
}
case class Left[+E](get: E) extends Either[E, Nothing]
case class Right[+A](get: A) extends Either[Nothing, A]

object Either {
  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(x => x)

  def traverse[E, A, B](as: List[A])(
    f: A => Either[E, B]): Either[E, List[B]] = as match {
    case Nil => Right(Nil)
    case h :: t => f(h).map2(traverse(t)(f))(_ :: _)
  }

  def map2L[E, A, B, C](a: Either[List[E], A], b: Either[List[E], B])(f: (A, B) => C): Either[List[E], C] =
    (a, b) match {
      case (Left(ale), Left(ble)) => Left(ale ::: ble)
      case (Left(ale), _) => Left(ale)
      case (_, Left(ble)) => Left(ble)
      case _ => a.map2(b)(f)
    }

  def sequenceL[E, A](es: List[Either[List[E], A]]): Either[List[E], List[A]] = traverseL(es)(x => x)

  def traverseL[E, A, B](as: List[A])(
    f: A => Either[List[E], B]): Either[List[E], List[B]] = as match {
    case Nil => Right(Nil)
    case h :: t => map2L(f(h), traverseL(t)(f))(_ :: _)
  }

}