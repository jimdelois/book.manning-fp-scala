package ch3

import org.scalatest._

/** @version 1.1.0 */
class ChapterExercisesTest extends FunSuite with Matchers {

  /**
    * Exercise 3.1
    *
    * What will be the result of the following match expression?
    *
    * val x = List(1,2,3,4,5) match {
    *   case Cons(x, Cons(2, Cons(4, _))) => x
    *   case Nil => 42
    *   case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y case Cons(h, t) => h + sum(t)
    *   case _ => 101
    * }
    */
  test("Exercise 3.1: Match Exercise") {

    val x = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }

    /**
      * Thought process:
      *
      * case 1: would miss on not having the value "3" in place
      * case 2: x isn't Nil List, so would miss
      * case 3: x matches into 1, y into 2, then match on 3, 4, then wildcard. MATCH
      *         So the value would be x + y = 3
      * case 4: Would also match, but case 3 hits first. Otherwise, value 15
      * case 5: Would also match, but case 3 hits first. Otherwise, value 101
      */
    x should be (3)

    /**
      * To verify my above thinking, another test that switches cases 3 & 4
      */
    val y = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(h, t) => h + List.sum(t)
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case _ => 101
    }

    y should be (15)
  }
}
