package ch4

import org.scalatest._

/** @version 1.1.0 */
class ChapterExercisesTest extends FunSuite with Matchers {

  /**
    * Exercise 4.2
    *
    * Implement the variance function in terms of flatMap. If the mean
    * of a sequence is m, the variance is the mean of math.pow(x - m, 2)
    * for each element x in the sequence. See the definition of variance
    * on Wikipedia (http://mng.bz/0Qsr).
    */
  test("Exercise 4.2L Variance with flatMap") {
    ChapterExercises.variance(List(1,2,3,5,5,6,7,8)) should be (Some(5.234375))
    ChapterExercises.variance(List()) should be (None)
  }

  /**
    * Section 4.3.2
    *
    * Lifting an Exception-oriented method
    */
  test("Section 4.3.2: Lift") {
    ChapterExercises.lift(math.abs)(Some(-5)) should be(Some(5))
  }

  /**
    * Section 4.3.2
    */
  test("Section 4.3.2: Lift Result") {
    ChapterExercises.liftResult(math.abs)(-5) should be(Some(5))
  }

  /**
    * Exercise 4.3
    *
    * Write a generic function map2 that combines two Option values using a
    * binary function. If either Option value is None, then the return
    * value is too.
    */
  test("Exercise 4.3: map2") {
    def fullName(first: String, last:String): String = s"$first $last"

    ChapterExercises.map2(Some("Jim"), Some("DeLois"))(fullName) should be (Some("Jim DeLois"))
    ChapterExercises.map2(None, Some("DeLois"))(fullName) should be (None)
    ChapterExercises.map2(Some("Jim"), None)(fullName) should be (None)

    ChapterExercises.map2Hinted(Some("Jim"), Some("DeLois"))(fullName) should be (Some("Jim DeLois"))
    ChapterExercises.map2Hinted(None, Some("DeLois"))(fullName) should be (None)
    ChapterExercises.map2Hinted(Some("Jim"), None)(fullName) should be (None)
  }

  /**
    * Exercise 4.4
    *
    * Write a function sequence that combines a list of Options into one Option containing
    * a list of all the Some values in the original list. If the original list contains
    * None even once, the result of the function should be None; otherwise the result should
    * be Some with a list of all the values.
    */
  test("Exercise 4.4: sequence") {
    ChapterExercises.sequence(List(Some(1), Some(2), Some(3))) should be (Some(1, 2, 3))
    ChapterExercises.sequence(List(Some(1), None, Some(3))) should be (None)
  }
}
