package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  // Exercise 3.25
  def size[A](t: Tree[A]): Int = {
    def go(t: Tree[A], s: Int): Int =
      t match {
        case Leaf(v) => s + 1
        case Branch(l, r) => go(l, s) + go(r, s) + 1
      }

    go(t, 0)
  }

  // Exercise 3.26
  def maximum(t: Tree[Int]): Int = {
    def go(t: Tree[Int], m: Int): Int =
      t match {
        case Leaf(v) => v max m
        case Branch(l, r) => go(l, m) max go(r, m)
      }

    go(t, 0)
  }

  // Exercise 3.27
  def depth[A](t: Tree[A]): Int = {
    def go(t: Tree[A], d: Int): Int =
      t match {
        case Leaf(v) => d + 1
        case Branch(l, r) => go(l, d)+1 max go(r, d)+1
      }

    go(t, 0)
  }

  // Exercise 3.28
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = 
    t match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l, r) => Branch(map(l)(f),map(r)(f))
    }

  // Exercose 3.29
  def fold[A,B](t: Tree[A])(l: A => B)(b: (B,B) => B): B = sys.error("todo")

}