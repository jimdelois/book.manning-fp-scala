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
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r) + 1
  }

  /**
    * Exercise 3.26
    *
    * Write a function maximum that returns the maximum element in a Tree[Int].
    * (Note: In Scala, you can use x.max(y) or x max y to compute the maximum
    * of two integers x andy.)
    */
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(value) => value
    case Branch(l, r) => maximum(l).max(maximum(r))
  }

  /**
    * Exercise 3.27
    *
    * Write a function depth that returns the maximum path length from
    * the root of a tree to any leaf.
    */
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + depth(l).max(depth(r))
  }

  /**
    * Exercise 3.28
    *
    * Write a function map, analogous to the method of the same name on
    * List, that modi- fies each element in a tree with a given function.
    */
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(value) => Leaf[B](f(value))
    case Branch(l, r) => Branch[B](map(l)(f), map(r)(f))
  }

  /**
    * Exercise 3.29
    *
    * Generalize size, maximum, depth, and map, writing a new function
    * fold that abstracts over their similarities. Reimplement them in terms
    * of this more general function. Can you draw an analogy between this
    * fold function and the left and right folds for List?
    */
  def fold[A,B](t: Tree[A])(leafFn: A => B)(branchFn: (B,B) => B): B = t match {
    case Leaf(value) => leafFn(value)
    case Branch(l, r) => branchFn(fold(l)(leafFn)(branchFn), fold(r)(leafFn)(branchFn))
  }

  /**
    * Exercise 3.29
    */
  def sizeFold[A](t: Tree[A]): Int = {
    fold(t)(_ => 1)((l, r) => l + r + 1)
  }

  /**
    * Exercise 3.29
    */
  def maximumFold(t: Tree[Int]): Int = {
    fold(t)(value => value)((l, r) => l.max(r))
  }

  /**
    * Exercise 3.29
    */
  // TODO: Complete depthFold

  /**
    * Exercise 3.29
    */
  // TODO: Complete mapFold
}
