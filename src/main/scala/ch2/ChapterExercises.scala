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
}
