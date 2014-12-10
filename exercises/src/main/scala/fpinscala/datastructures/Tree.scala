package fpinscala.datastructures

sealed trait Tree[+A]
// case object Empty extends Tree[Nothing]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  /** 
   *  Number of nodes in a tree
   */
  def size[A](t: Tree[A]): Int = t match {
    // case Empty => 0
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  /** Maximum elements in a tree of nodes in a tree
   */
  def maximum(ti: Tree[Int]): Int = {
    // assert(ti != Empty, "tree must not be empty")
    ti match {
      case Leaf(v) => v
      case Branch(l, r) => maximum(l) max maximum(r)
      case _ => Int.MinValue
    }
  }

  /** Depth of a tree
   */
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
    case _ => 0
  }

  /** 
   *  Map for Tree
   */
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    // case Empty => Empty
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }
  
  /**
   * FlatMap for Tree
   */
  def flatMap[A, B](t: Tree[A])(f: A => Tree[B]): Tree[B] = t match {
    // case Empty => Empty
    case Leaf(v) => f(v)
    case Branch(l, r) => Branch(flatMap(l)(f), flatMap(r)(f))
  }
  
  /**
   * Map2 for Tree
   */
  def map2[A, B, C](ta: Tree[A], tb: Tree[B])(f: (A, B) => C): Tree[C] =
    flatMap(ta)(a => (map(tb)(b => f(a,b))))

  /** Fold tree
   */
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = {
    // assert(t != Empty, "tree must not be empty")
    t match {
      case Leaf(v) => f(v)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }
  }
  
  /**
   * Add value
   */
  def add[A](t: Tree[A])(value: A): Tree[A] = t match {
    // case Empty => Leaf(value)
    case l: Leaf[A] => Branch(l, Leaf(value))
    case b: Branch[A] => Branch(b, Leaf(value))
  }
  /**
   * Fill Tree with value
   * @size
   * @value
   */
  def fill[A](size: Int)(value: A): Tree[A] = {
    assert(size > 0, "tree must have at least one element")
    @annotation.tailrec
    def go(t: Tree[A], size: Int, value: A): Tree[A] = {
      if (size == 0) t
      else if (size%2 == 0) go(Branch(t, Leaf(value)), size-1, value)
      else go(Branch(Leaf(value), t), size-1, value)
    }
    go(Leaf(value), size-1, value) 
  }
  
  /**
   * Converts List to Tree
   */
  def toTree[A](l: List[A]): Tree[A] = {
    assert(List.length(l) > 0, "tree must have at least one element")
    List.foldLeft(List.tail(l), Leaf(List.head(l)): Tree[A])(add(_)(_))
  }

}