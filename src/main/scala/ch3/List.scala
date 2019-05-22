package ch3

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  // Additional "Constructor"
  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ints: List[Int]): Int = ints match {
    case Nil => 1
    case Cons(x, xs) => x * product(xs)
  }

  /**
    * Exercise 3.2
    *
    * Implement the function tail for removing the first element of a List.
    * Note that the function takes constant time. What are different choices
    * you could make in your implementation if the List is Nil? We’ll return
    * to this question in the next chapter.
    */
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  /**
    * Exercise 3.3
    *
    * Using the same idea, implement the function setHead for replacing the
    * first element of a List with a different value.
    */
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => List[A](h)
    case Cons(_, xs) => Cons[A](h, xs)
  }

  /**
    * Exercise 3.4
    *
    * Generalize tail to the function drop, which removes the first n elements
    * from a list. Note that this function takes time proportional only
    * to the number of elements being dropped—we don’t need to make a copy of
    * the entire List.
    *
    */
  def drop[A](l: List[A], n: Int): List[A] = {
    @scala.annotation.tailrec
    def go(list: List[A], count: Int): List[A] = {
      if (count > n) list
      else go(tail(list), count+1)
    }

    go(l, 1)
  }
}
