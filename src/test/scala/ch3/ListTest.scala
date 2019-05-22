package ch3

import org.scalatest._

/**
  * A test class aside from Exercises to flex the methods of the
  * List structure and the companion class
  *
  * @version 1.1.0
  */
class ListTest extends FunSuite with Matchers {


  test("Sum") {

    List.sum(Nil) should be (0)
    List.sum(List()) should be (0)

    List.sum(List[Int](4)) should be (4)
    List.sum(List[Int](3, 4, 5)) should be (12)
    List.sum(Cons[Int](3, Nil)) should be (3)
    List.sum(Cons[Int](3, Cons[Int](4, Nil))) should be (7)
    List.sum(Cons[Int](3, List[Int](4, 5))) should be (12)
    List.sum(Cons[Int](3, Cons[Int](4, Cons[Int](5, Nil)))) should be (12)
  }

  test("Product") {

    List.product(Nil) should be (1)
    List.product(List()) should be (1)

    List.product(List[Int](4)) should be (4)
    List.product(List[Int](3, 4, 5)) should be (60)
    List.product(Cons[Int](3, Nil)) should be (3)
    List.product(Cons[Int](3, Cons[Int](4, Nil))) should be (12)
    List.product(Cons[Int](3, List[Int](4, 5))) should be (60)
    List.product(Cons[Int](3, Cons[Int](4, Cons[Int](5, Nil)))) should be (60)
  }

  /**
    * Exercise 3.2
    *
    * Implement the function tail for removing the first element of a List.
    * Note that the function takes constant time. What are different choices
    * you could make in your implementation if the List is Nil? We’ll return
    * to this question in the next chapter.
    */
  test("Exercise 3.2: Tail") {

    List.tail(List()) should be (Nil)
    List.tail(Nil) should be (List())

    List.tail(List[Int](1, 2, 3)) should be (List[Int](2, 3))
    List.tail(Cons[Int](1, Nil)) should be (Nil)
    List.tail(Cons[Int](1, Cons[Int](2, List[Int](3)))) should be (List[Int](2, 3))

    List.tail(List[String]("A", "Bee", "See")) should be (List[String]("Bee", "See"))
  }
}
