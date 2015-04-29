package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  // Exercise 5.1
  def toList: List[A] =
    foldRight(List[A]())((a, b) => a::b)

  // Exercise 5.2
  def take(n: Int): Stream[A] = {
    def go(i: Int, s: Stream[A]): Stream[A] =
      if (i == 0) Empty
      else s match {
        case Cons(h, t) => Cons(h, () => go(i-1, t()))
        case Empty => s
      }

    go(n, this)
  }

  // Exercise 5.2
  def drop(n: Int): Stream[A] = {
    @annotation.tailrec
    def go(i: Int, s: Stream[A]): Stream[A] =
      if (i == 0) s
      else s match {
        case Empty => s
        case Cons(h, t) => go(i-1, t())
      }

    go(n, this)
  }

  // Exercise 5.3
  def takeWhile(p: A => Boolean): Stream[A] = 
    this match {
      case Cons(h, t) if (p(h())) => cons(h(), t() takeWhile p)
      case _ => empty
    }

  // Exercise 5.4
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  // Exercise 5.5
  def takeWhile_55(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h,t) => if (p(h)) cons(h, t) else empty)

  // Exercise 5.6
  def headOption: Option[A] = sys.error("todo")

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h,t) => cons(f(h), t))

  def filter(f: A => Boolean): Stream[A] = 
    foldRight(empty[A])((h,t) => if (f(h)) cons(h, t) else t)

  def append(s: Stream[A]): Stream[A] = 
    foldRight(s)((h, t) => cons(h, t))



  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")
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
  def from(n: Int): Stream[Int] = sys.error("todo")

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = sys.error("todo")
}