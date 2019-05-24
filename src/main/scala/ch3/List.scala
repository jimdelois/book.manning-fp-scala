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

  /**
    * Exercise 3.11
    *
    * Write sum, product, and a function to compute the length of a list using foldLeft.
    */
  def sumByFoldingLeft(l: List[Int]): Int = foldLeft(l, 0)(_ + _)
  def productByFoldingLeft(l: List[Int]): Int = foldLeft(l, 1)(_ * _)
  def lengthByFoldingLeft[A](l: List[A]): Int = foldLeft(l, 0)((agg, _) => agg + 1)

  /**
    * Exercise 3.12
    *
    * Write a function that returns the reverse of a list (given List(1,2,3) it
    * returns List(3,2,1)). See if you can write it using a fold.
    */
  def reverse[A](l: List[A]): List[A] = {
    foldLeft[A,List[A]](l, Nil)((agg, x) => Cons(x, agg))
  }

  /**
    * Exercise 3.13
    *
    * Hard: Can you write foldLeft in terms of foldRight? How about the other way
    * around? Implementing foldRight via foldLeft is useful because it lets us implement
    * foldRight tail-recursively, which means it works even for large lists without
    * overflow- ing the stack.
    */
  def foldRightTail[A,B](l: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(l), z)((b: B, a: A) => f(a, b))
  }

  /**
    * Exercise 3.14
    *
    * Implement append in terms of either foldLeft or foldRight.
    */
  def append[A](xs: List[A], ys: List[A]): List[A] = {
    foldRight(xs, ys)((x, agg) => Cons(x, agg))
    // Or, for tail recursion (but an added reverse):
    // foldLeft(reverse(xs))((agg, x) => Cons(agg, x))
  }

  /**
    * Exercise 3.15
    *
    * Hard: Write a function that concatenates a list of lists into a single list.
    * Its runtime should be linear in the total length of all lists. Try to use
    * functions we have already defined.
    */
  def concat[A](l: List[List[A]]): List[A] = {
    foldLeft[List[A],List[A]](l, Nil)(append)
  }

  /**
    * Exercise 3.16
    *
    * Write a function that transforms a list of integers by adding 1 to each
    * element. (Reminder: this should be a pure function that returns
    * a new List!)
    */
  def addToEach(l: List[Int], n: Int): List[Int] = {
    reverse(foldLeft[Int,List[Int]](l, Nil)((l, x) => Cons(x+n, l)))
  }

  /**
    * Exercise 3.17
    *
    * Write a function that turns each value in a List[Double] into a String.
    * You can use the expression d.toString to convert some d: Double to a String.
    */
  def dubString(l: List[Double]): List[String] = {
    foldRight[Double,List[String]](l, Nil)((x, l) => Cons(x.toString, l))
  }
}
