package ch3

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  /**
    * Exercise 3.25
    *
    * Write a function size that counts the number of
    * nodes (leaves and branches) in a tree.
    */
  def size[A](t: Tree[A]): Int = {
    def go(t: Tree[A], agg: Int): Int = t match {
      case Leaf(_) => agg
      case Branch(l, r) => go(l, go(r, agg + 1) + 1)
    }
    go(t,1)
  }

  /**
    * Exercise 3.26
    *
    * Write a function maximum that returns the maximum element in a Tree[Int].
    * (Note: In Scala, you can use x.max(y) or x max y to compute the maximum
    * of two integers x andy.)
    */
  def maximum(t: Tree[Int]): Int = {
    def go(t: Tree[Int], max: Int): Int = t match {
      case Leaf(value) => value.max(max)
      case Branch(l,r) => go(l,max).max(go(r,max))
    }
    go(t,0)
  }
}
