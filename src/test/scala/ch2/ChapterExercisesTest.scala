package ch2

import org.scalatest._

/** @version 1.1.0 */
class ChapterExercisesTest extends FunSuite with Matchers {

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

  test("Exercise 2.2: Generic Array Sort") {
    def compInt(a: Int, b:Int): Boolean = a <= b

    ChapterExercises.isSorted[Int](Array(3, 4, 5), compInt) should be (true)
    ChapterExercises.isSorted[Int](Array(3, 5, 4), compInt) should be (false)

    def compString(a: String, b:String): Boolean = a <= b

    ChapterExercises.isSorted[String](Array("Mom", "Pop"), compString) should be (true)
    ChapterExercises.isSorted[String](Array("Pop", "Mom"), compString) should be (false)
  }

  test("Exercise 2.3: Basic Currying") {
    def add(x: Int, y: Int): Int = x + y

    val addCurried = ChapterExercises.curry[Int, Int, Int](add)

    addCurried(5)(2) should be (7)
    addCurried(2)(11) should be (13)
  }
}
