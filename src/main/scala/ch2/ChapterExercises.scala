package ch2

import scala.annotation.tailrec

object ChapterExercises {
  /**
    * Exercise 2.1
    *
    * Write a recursive function to get the nth Fibonacci number (http://mng.bz/C29s).
    * The first two Fibonacci numbers are 0 and 1. The nth number is always the sum of the previous two.
    * The sequence begins 0, 1, 1, 2, 3, 5.
    * Your definition should use a local tail-recursive function.
    */
  def fib(n: Int): Int = {
    @tailrec
    def go(idx: Int, prev: Int, curr: Int): Int =
      if (idx > n) prev
      else go(idx+1, curr, prev+curr)

    if (n <= 0) -1
    else if (n == 1) 0
    else go(2, 0, 1)
  }

  /**
    * Exercise 2.2
    *
    * Implement isSorted, which checks whether an Array[A] is sorted according to a
    * given comparison function:
    */
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @tailrec
    def go(idx: Int): Boolean =
      if (idx+1 == as.length) true
      else if (ordered(as(idx), as(idx+1))) go(idx+1)
      else false

    go(0)
  }

  /**
    * Exercise 2.3
    *
    * Let’s look at another example, currying, which converts a function f of two arguments
    * into a function of one argument that partially applies f. Here again there’s only one
    * implementation that compiles. Write this implementation.
    */
  def curry[A,B,C](f: (A, B) => C): A => B => C = {
    a: A => b: B => f(a, b)
  }
}
