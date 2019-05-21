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
}
