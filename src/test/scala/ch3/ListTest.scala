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

  /**
    * Exercise 3.3
    *
    * Using the same idea, implement the function setHead for replacing the
    * first element of a List with a different value.
    */
  test("Exercise 3.3: setHead") {

    List.setHead(Nil, 4) should be (List[Int](4))
    List.setHead(List[String]("A", "B", "C"), "D") should be (List[String]("D", "B", "C"))
    List.setHead(List[Int](3), 5) should be (List[Int](5))
  }

  /**
    * Exercise 3.4
    *
    * Generalize tail to the function drop, which removes the first n elements
    * from a list. Note that this function takes time proportional only
    * to the number of elements being dropped—we don’t need to make a copy of
    * the entire List.
    *
    */
  test("Exercise 3.4: drop") {

    List.drop(Nil, 4) should be (Nil)
    List.drop(List[Int](1, 2, 3, 4, 5), 3) should be (List[Int](4, 5))
    List.drop(List[Int](1, 2), 3) should be (Nil)
  }

  /**
    * Exercise 3.5
    *
    * Implement dropWhile, which removes elements from the List prefix as long
    * as they match a predicate.
    */
  test("Exercise 3.5: dropWhile") {
    def dropper: Int => Boolean = (x: Int) => x % 2 == 0

    List.dropWhile(Nil, dropper) should be (Nil)
    List.dropWhile(List[Int](2, 4, 6, 3, 2, 4), dropper) should be (List[Int](3, 2, 4))
  }

  /**
    * Exercise 3.6
    *
    * Not everything works out so nicely. Implement a function, init, that
    * returns a List consisting of all but the last element of a List. So,
    * given List(1,2,3,4), init will return List(1,2,3). Why can’t this
    * function be implemented in constant time like tail?
    */
  test("Exercise 3.6: Init") {

    List.init(Nil) should be (Nil)
    List.init(List[Int](1, 2, 3, 4)) should be (List[Int](1, 2, 3))
    List.init(List[Int](4)) should be (Nil)
  }

  test("Sum By Folding Right") {

    List.sumByFoldingRight(Nil) should be (0)
    List.sumByFoldingRight(List()) should be (0)

    List.sumByFoldingRight(List[Int](4)) should be (4)
    List.sumByFoldingRight(List[Int](3, 4, 5)) should be (12)
    List.sumByFoldingRight(Cons[Int](3, Nil)) should be (3)
    List.sumByFoldingRight(Cons[Int](3, Cons[Int](4, Nil))) should be (7)
    List.sumByFoldingRight(Cons[Int](3, List[Int](4, 5))) should be (12)
    List.sumByFoldingRight(Cons[Int](3, Cons[Int](4, Cons[Int](5, Nil)))) should be (12)
  }

  test("Product By Folding Right") {

    List.productByFoldingRight(Nil) should be (1)
    List.productByFoldingRight(List()) should be (1)

    List.productByFoldingRight(List[Int](4)) should be (4)
    List.productByFoldingRight(List[Int](3, 4, 5)) should be (60)
    List.productByFoldingRight(Cons[Int](3, Nil)) should be (3)
    List.productByFoldingRight(Cons[Int](3, Cons[Int](4, Nil))) should be (12)
    List.productByFoldingRight(Cons[Int](3, List[Int](4, 5))) should be (60)
    List.productByFoldingRight(Cons[Int](3, Cons[Int](4, Cons[Int](5, Nil)))) should be (60)
  }

  /**
    * Exercise 3.9
    *
    * Compute the length of a list using foldRight.
    */
  test("Exercise 3.9: Length using foldRight") {
    List.length(Nil) should be (0)
    List.length(List[Int](0)) should be (1)
    List.length(List[Int](-1)) should be (1)
    List.length(List[Int](1, 3, 3, 4, 5, 5)) should be (6)
    List.length(List[String]("A", "B")) should be (2)
  }

  /**
    * Exercise 3.11
    *
    * Write sum, product, and a function to compute the length of a list using foldLeft.
    */
  test("Exercise 3.11: Sum using foldLeft") {

    List.sumByFoldingLeft(Nil) should be (0)
    List.sumByFoldingLeft(List()) should be (0)

    List.sumByFoldingLeft(List[Int](4)) should be (4)
    List.sumByFoldingLeft(List[Int](3, 4, 5)) should be (12)
    List.sumByFoldingLeft(Cons[Int](3, Nil)) should be (3)
    List.sumByFoldingLeft(Cons[Int](3, Cons[Int](4, Nil))) should be (7)
    List.sumByFoldingLeft(Cons[Int](3, List[Int](4, 5))) should be (12)
    List.sumByFoldingLeft(Cons[Int](3, Cons[Int](4, Cons[Int](5, Nil)))) should be (12)
  }

  /**
    * Exercise 3.11 (Cont'd)
    */
  test("Exercise 3.11: Product using foldLeft") {

    List.productByFoldingLeft(Nil) should be (1)
    List.productByFoldingLeft(List()) should be (1)

    List.productByFoldingLeft(List[Int](4)) should be (4)
    List.productByFoldingLeft(List[Int](3, 4, 5)) should be (60)
    List.productByFoldingLeft(Cons[Int](3, Nil)) should be (3)
    List.productByFoldingLeft(Cons[Int](3, Cons[Int](4, Nil))) should be (12)
    List.productByFoldingLeft(Cons[Int](3, List[Int](4, 5))) should be (60)
    List.productByFoldingLeft(Cons[Int](3, Cons[Int](4, Cons[Int](5, Nil)))) should be (60)
  }

  /**
    * Exercise 3.11 (Cont'd)
    */
  test("Exercise 3.11: Length using foldLeft") {

    List.lengthByFoldingLeft(Nil) should be (0)
    List.lengthByFoldingLeft(List[Int](0)) should be (1)
    List.lengthByFoldingLeft(List[Int](-1)) should be (1)
    List.lengthByFoldingLeft(List[Int](1, 3, 3, 4, 5, 5)) should be (6)
    List.lengthByFoldingLeft(List[String]("A", "B")) should be (2)
  }

  /**
    * Exercise 3.12
    *
    * Write a function that returns the reverse of a list (given List(1,2,3) it
    * returns List(3,2,1)). See if you can write it using a fold.
    */
  test("Exercise 3.12: Reversing a List") {

    List.reverse(Nil) should be (Nil)
    List.reverse(List[Int](1, 2, 3)) should be (List[Int](3, 2, 1))
    List.reverse(List[String]("J", "i", "m")) should be (List[String]("m", "i", "J"))
  }

  /**
    * Exercise 3.13
    *
    * Hard: Can you write foldLeft in terms of foldRight? How about the other way
    * around? Implementing foldRight via foldLeft is useful because it lets us implement
    * foldRight tail-recursively, which means it works even for large lists without
    * overflow- ing the stack.
    */
  test("Exercise 3.13: foldRightTail") {

    List.foldRightTail(List[Int](3, 4, 5), 0)(_ + _) should be (12)
    List.foldRightTail(List[Int](3, 4, 5), 1)(_ * _) should be (60)

    List.foldRightTail(List[Int](1, 3, 3, 4, 5, 5), 0)((_, agg) => agg + 1) should be (6)
    List.foldRightTail(List[String]("A", "B"), 0)((_, agg) => agg + 1) should be (2)
  }

  /**
    * Exercise 3.14
    *
    * Implement append in terms of either foldLeft or foldRight.
    */
  test("Exercise 3.14: Append") {
    List.append(
      List[String]("A", "B", "C"),
      List[String]("D", "E", "F")
    ) should be (List[String]("A", "B", "C", "D", "E", "F"))
  }

  /**
    * Exercise 3.15
    *
    * Hard: Write a function that concatenates a list of lists into a single list.
    * Its runtime should be linear in the total length of all lists. Try to use
    * functions we have already defined.
    */
  test("Exercise 3.15: Concat") {
    List.concat(List[List[String]](
      List[String]("A", "B"),
      List[String]("C"),
      List[String]("D", "E")
    )) should be (List[String]("A", "B", "C", "D", "E"))
  }

  /**
    * Exercise 3.16
    *
    * Write a function that transforms a list of integers by adding 1 to each
    * element. (Reminder: this should be a pure function that returns
    * a new List!)
    */
  test("Excercise 3.16: Add One") {
    // I obviously genericized this, but the point is the same
    List.addToEach(List[Int](1, 2, 3), 4) should be (List(5, 6, 7))
  }

  /**
    * Exercise 3.17
    *
    * Write a function that turns each value in a List[Double] into a String.
    * You can use the expression d.toString to convert some d: Double to a String.
    */
  test("Exercise 3.17: Double to String") {
    List.dubString(
      List[Double](2.3, 4.5, 5.22)
    ) should be (List[String]("2.3", "4.5", "5.22"))
  }

  /**
    * Exercise 3.18
    *
    * Write a function map that generalizes modifying each element in a list while
    * maintaining the structure of the list. Here is its signature:
    */
  test("Exercise 3.18: Map" ) {
    List.map(
      List[Double](2.3, 4.5, 5.22)
    )(_.toString()) should be (List[String]("2.3", "4.5", "5.22"))
  }

  /**
    * Exercise 3.19
    *
    * Write a function filter that removes elements from a list unless they satisfy a
    * given predicate. Use it to remove all odd numbers from a List[Int].
    */
  test("Exercise 3.19: Filter") {
    List.filter(List[Int](1,2,3,4,5))(_ % 2 == 0) should be (List[Int](2,4))
  }

  /**
    * Exercise 3.20
    *
    * Write a function flatMap that works like map except that the function given will
    * return a list instead of a single result, and that list should be inserted into
    * the final resulting list.
    *
    * For instance, flatMap(List(1,2,3))(i => List(i,i)) should result in
    * List(1,1,2,2,3,3)
    */
  test("Exercise 3.20: flatMap") {
    List.flatMap(List[Int](1,2,3))(i => List(i,i)) should be (List[Int](1,1,2,2,3,3))
  }

  /**
    * Exercise 3.21
    *
    * Use flatMap to implement filter.
    */
  test("Exercise 3.21: Filter via flatMap") {
    List.filterFlatMap(List[Int](1,2,3,4,5))(_ % 2 == 0) should be (List[Int](2,4))
  }

  /**
    * Exercise 3.22
    *
    * Write a function that accepts two lists and constructs a new list by adding corresponding
    * elements. For example, List(1,2,3) and List(4,5,6) become List(5,7,9).
    */
  test("Exercise 3.22: Integer Addition Zip") {
    List.zipAdd(List[Int](1,2,3),List[Int](4,5,6)) should be (List[Int](5,7,9))
  }

  /**
    * Exercise 3.23
    *
    * Generalize the function you just wrote so that it’s not specific to integers or
    * addition. Name your generalized function zipWith.
    */
  test("Exercise 3.23: Zip With") {
    List.zipWith(
      List[Int](1,2,3),
      List[Int](4,5,6)
    )((x: Int, y:Int) => x + y) should be (List[Int](5,7,9))
  }

}
