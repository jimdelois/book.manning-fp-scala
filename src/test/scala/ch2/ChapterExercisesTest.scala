package ch2

import org.scalatest._

/** @version 1.1.0 */
class ChapterExercisesTest extends FunSuite with Matchers {

  /**
    * Exercise 2.1
    *
    * Write a recursive function to get the nth Fibonacci number (http://mng.bz/C29s).
    * The first two Fibonacci numbers are 0 and 1. The nth number is always the sum of the previous two.
    * The sequence begins 0, 1, 1, 2, 3, 5.
    * Your definition should use a local tail-recursive function.
    */
  test("Exercise 2.1: Fibonacci recursion") {

    // There is no "zeroth" number
    ChapterExercises.fib(0) should be (-1)
    ChapterExercises.fib(-5) should be (-1)

    // Base cases
    ChapterExercises.fib(1) should be (0)
    ChapterExercises.fib(2) should be (1)

    // Recursive cases
    ChapterExercises.fib(3) should be (1)
    ChapterExercises.fib(4) should be (2)
    ChapterExercises.fib(5) should be (3)
    ChapterExercises.fib(6) should be (5)
    ChapterExercises.fib(7) should be (8)
    ChapterExercises.fib(8) should be (13)
  }

  /**
    * Exercise 2.2
    *
    * Implement isSorted, which checks whether an Array[A] is sorted according to a
    * given comparison function:
    */
  test("Exercise 2.2: Generic Array Sort") {
    def compInt(a: Int, b:Int): Boolean = a <= b

    ChapterExercises.isSorted[Int](Array(3, 4, 5), compInt) should be (true)
    ChapterExercises.isSorted[Int](Array(3, 5, 4), compInt) should be (false)

    def compString(a: String, b:String): Boolean = a <= b

    ChapterExercises.isSorted[String](Array("Mom", "Pop"), compString) should be (true)
    ChapterExercises.isSorted[String](Array("Pop", "Mom"), compString) should be (false)
  }

  /**
    * Exercise 2.3
    *
    * Let’s look at another example, currying, which converts a function f of two arguments
    * into a function of one argument that partially applies f. Here again there’s only one
    * implementation that compiles. Write this implementation.
    */
  test("Exercise 2.3: Basic Currying") {
    def add(x: Int, y: Int): Int = x + y

    val addCurried = ChapterExercises.curry[Int, Int, Int](add)

    addCurried(5)(2) should be (7)
    addCurried(2)(11) should be (13)
  }

  /**
    * Exercise 2.4
    *
    * Implement uncurry, which reverses the transformation of curry. Note that
    * since => associates to the right, A => (B => C) can be written as A => B => C.
    */
  test("Exercise 2.4: Uncurrying") {

    def addCurried(x: Int): Int => Int = (y: Int) => x + y

    val add = ChapterExercises.uncurry[Int, Int, Int](addCurried)

    add(5, 2) should be (7)
    add(2, 11) should be (13)
  }

  /**
    * Exercise 2.5
    *
    * Implement the higher-order function that composes two functions.
    */
  test("Exercise 2.5: Composition") {
    def multiplyByTwo (a: Int): Int = a * 2
    def addOne(a: Int): Int = a + 1

    val fn = ChapterExercises.compose(addOne, multiplyByTwo)
    fn(3) should be (7)
  }
}
