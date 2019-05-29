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
}
