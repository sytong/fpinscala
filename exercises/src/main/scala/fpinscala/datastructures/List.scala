package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  } 
  
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  
  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42 
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101 
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  
  def sum2(ns: List[Int]) = 
    foldRight(ns, 0)((x,y) => x + y)
  
  def product2(ns: List[Double]) = 
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  // Exercise 3.2
  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(h, t) => t
    }

  // Exercise 3.3
  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil => Cons(h, Nil)
      case Cons(x, xs) => Cons(h, xs)
    }

  // Exercise 3.4
  def drop[A](l: List[A], n: Int): List[A] = 
    if (n == 0) l
    else l match {
      case Nil => Nil
      case Cons(h, t) => drop(t, n-1)
    }

  // Exercise 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case Cons(h, t) => 
        if (f(h)) dropWhile(t, f)
        else l
    }

  // Exercise 3.6
  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(h, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

  // Exercise 3.9
  def length[A](l: List[A]): Int =
    foldRight(l, 0)((x ,y) => y + 1)

  // Exercise 3.10
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def go(y: B, xs: List[A]): B =
      xs match {
        case Nil => y
        case Cons(h, t) => go(f(y, h), t)
      }

    go(z, l)
  }

  // Exercise 3.11
  def sum311(ns: List[Int]) = 
    foldLeft(ns, 0)((y, x) => y + x)
  
  // Exercise 3.11
  def product311(ns: List[Double]) = 
    foldLeft(ns, 1.0)((y, x) => y * x)

  // Exercise 3.11
  def length311[A](l: List[A]): Int =
    foldLeft(l, 0)((y, x) => y + 1)
  
  // Exercise 3.12
  def reverse[A](l: List[A]): List[A] =
    foldRight(l, Nil:List[A])((x, y) => append(y, Cons(x, Nil)))

  // Exercise 3.13
  def foldLeft313[A,B](l: List[A], z: B)(f: (B, A) => B): B = sys.error("todo")
    
  // Exercise 3.13
  def foldRight313[A,B](as: List[A], z: B)(f: (A, B) => B): B = sys.error("todo")

  // Exercise 3.14
  def appendFoldRight[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((x,y) => Cons(x, y))

  // Exercise 3.14
  def appendFoldLeft[A](a1: List[A], a2: List[A]): List[A] = sys.error("todo")

  // Exercise 3.15
  def concatenate[A](ll: List[List[A]]): List[A] = sys.error("todo")

  // Exercise 3.16
  def addOne(l: List[Int]): List[Int] =
    l match {
      case Nil => Nil
      case Cons(h, t) => Cons(h+1, addOne(t))
    }

  // Exercise 3.17
  def doubleToString(l: List[Double]): List[String] =
    l match {
      case Nil => Nil
      case Cons(h, t) => Cons(h.toString, doubleToString(t))
    }

  // Exercise 3.18
  def map[A,B](l: List[A])(f: A => B): List[B] =
    l match {
      case Nil => Nil
      case Cons(h, t) => Cons(f(h), map(t)(f))
    }

  // Exercise 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    as match {
      case Nil => Nil
      case Cons(h, t) =>
        if (f(h)) Cons(h, filter(t)(f))
        else filter(t)(f)
    }

  // Exercise 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = sys.error("todo")
}