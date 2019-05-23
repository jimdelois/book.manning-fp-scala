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
  @scala.annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Cons(_, xs) if n > 0 => drop(xs, n - 1)
    case _ => l
  }

  /**
    * Exercise 3.5
    *
    * Implement dropWhile, which removes elements from the List prefix as long
    * as they match a predicate.
    */
  @scala.annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _ => l
  }

  /**
    * Exercise 3.6
    *
    * Not everything works out so nicely. Implement a function, init, that
    * returns a List consisting of all but the last element of a List. So,
    * given List(1,2,3,4), init will return List(1,2,3). Why can’t this
    * function be implemented in constant time like tail?
    */
  def init[A](l: List[A]): List[A] = l match {
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
    case _ => l
  }

  private def foldRight[A, B](l: List[A], default: B)(f: (A, B) => B): B = l match {
    case Nil => default
    case Cons(x, xs) => f(x, foldRight(xs, default)(f))
  }

  def sumByFoldingRight(l: List[Int]): Int = foldRight(l, 0)(_ + _)

  // TODO: Can this support an "early exit" if it encounters a zero?
  //       Try to avoid further looping if so, as posited by p40
  def productByFoldingRight(l: List[Int]): Int = foldRight(l, 1)(_ * _)

  /**
    * Exercise 3.9
    *
    * Compute the length of a list using foldRight.
    */
  def length[A](l: List[A]): Int = foldRight(l, 0)((_, agg) => 1 + agg)

  /**
    * Exercise 3.10
    *
    * Our implementation of foldRight is not tail-recursive and will result in a
    * StackOverflowError for large lists (we say it’s not stack-safe). Convince
    * yourself that this is the case, and then write another general list-recursion
    * function, foldLeft, that is tail-recursive, using the techniques we
    * discussed in the previous chapter.
    */

  private def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    @scala.annotation.tailrec
    def go(l: List[A], agg: B): B = l match {
      case Nil => agg
      case Cons(x, xs) => go(xs, f(agg, x))
    }
    go(l, z)
  }
}
