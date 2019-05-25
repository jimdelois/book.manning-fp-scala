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

  val exampleTreeInt:Tree[Int] = Branch(
    Branch(
      Branch(
        Leaf(25),
        Leaf(16)
      ),
      Branch(
        Leaf(8),
        Leaf(13)
      )
    ),
    Branch(
      Leaf(19),
      Leaf(22)
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

  /**
    * Exercise 3.26
    *
    * Write a function maximum that returns the maximum element in a Tree[Int].
    * (Note: In Scala, you can use x.max(y) or x max y to compute the maximum
    * of two integers x andy.)
    */
  test("Exercise 3.26: Maximum") {
    Tree.maximum(exampleTreeInt) should be (25)
  }

  /**
    * Exercise 3.27
    *
    * Write a function depth that returns the maximum path length from
    * the root of a tree to any leaf.
    */
  test("Exercise 3.27 Depth") {
    Tree.depth(exampleTree) should be (2)
  }
}
