package ch4

import org.scalatest._

/**
  * A test class aside from Exercises to flex the methods of the
  * Option structure and companion classes
  *
  * @version 1.1.0
  */
class OptionTest extends FunSuite with Matchers {

  /**
    * Exercise 4.1
    *
    * Implement all of the functions [map, flatMap, getOrElse, orElse, filter]
    * on Option. As you implement each function, try to think about what it means
    * and in what situations you’d use it. We’ll explore when to use each of
    * these functions next.
    */
  test("Exercise 4.1: map") {
    Some[Double](2.88).map(_.toString()) should be (Some[String]("2.88"))
    None.map(_.toString()) should be (None)
  }

  /**
    * Exercise 4.1
    */
  test("Exercise 4.1: getOrElse") {
    Some[String]("Nice").getOrElse("Not Nice") should be ("Nice")
    None.getOrElse("Not Nice") should be("Not Nice")
  }
}
