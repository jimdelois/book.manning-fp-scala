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
    Some[Double](2.88).map(_.toString) should be (Some[String]("2.88"))
    None.map(_.toString) should be (None)
  }

  /**
    * Exercise 4.1
    */
  test("Exercise 4.1: getOrElse") {
    Some[String]("Nice").getOrElse("Not Nice") should be ("Nice")
    None.getOrElse("Not Nice") should be("Not Nice")
  }

  /**
    * Exercise 4.1
    */
  test("Exercise 4.1: flatMap") {
    Some[Double](2.88).flatMap(x => Some[String](x.toString)) should be (Some[String]("2.88"))
    Some[Double](2.88).flatMap(_ => None) should be (None)
  }

  /**
    * Exercise 4.1
    */
  test("Exercise 4.1: orElse") {
    None.orElse(Some[String]("Else!")) should be (Some[String]("Else!"))
    Some[String]("Some!").orElse(Some[String]("Else!")) should be (Some[String]("Some!"))
  }

  /**
    * Exercise 4.1
    */
  test("Exercise 4.1: filter") {
    Some[Int](3).filter(_ % 2 == 0) should be (None)
    Some[Int](3).filter(_ % 2 == 1) should be (Some[Int](3))
    None.filter(_ => true) should be (None)
    None.filter(_ => false) should be (None)
  }
}
