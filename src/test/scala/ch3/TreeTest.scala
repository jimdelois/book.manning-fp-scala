package ch3

import org.scalatest._

/**
  * A test class aside from Exercises to flex the methods of the
  * Tree structure and the companion class
  *
  * @version 1.1.0
  */
class TreeTest extends FunSuite with Matchers {

  val exampleTree:Tree[String] = Branch(
    Branch(
      Leaf("a"),
      Leaf("b")
    ),
    Branch(
      Leaf("c"),
      Leaf("d")
    )
  )

  /**
    * Exercise 3.25
    *
    * Write a function size that counts the number of
    * nodes (leaves and branches) in a tree.
    */
  test("Exercise 3.25: Size") {

    Tree.size(Branch(
      Leaf("a"),
      Leaf("b")
    )) should be (3)

    Tree.size(exampleTree) should be (7)
  }
}
