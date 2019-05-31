package ch4

object ChapterExercises {

  /**
    * Exercise 4.2
    *
    * Implement the variance function in terms of flatMap. If the mean
    * of a sequence is m, the variance is the mean of math.pow(x - m, 2)
    * for each element x in the sequence. See the definition of variance
    * on Wikipedia (http://mng.bz/0Qsr).
    */
  def variance(xs: Seq[Double]): Option[Double] = {
    def mean(xs: Seq[Double]): Option[Double] = {
      if (xs.isEmpty) None
      else Some(xs.sum / xs.length)
    }

    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  /**
    * Section 4.3.2
    *
    * Lifting an Exception-oriented method
    */
  def lift[A,B](f: A => B): Option[A] => Option[B] = {
    x:Option[A] => x.map(f)
  }

  /**
    * Section 4.3.2
    *
    * Based on realizing that the above is useful for lifting in unison BOTH
    * input and output, suppose we want to convert an Exception-oriented API
    * result to an Option range, without altering its domain.
    */
  def liftResult[A, B](f: A => B): A => Option[B] = {
    x:A => Some(f(x))
  }

  /**
    * Exercise 4.3
    *
    * Write a generic function map2 that combines two Option values using a
    * binary function. If either Option value is None, then the return
    * value is too.
    */
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a,b) match {
    case (None,_) => None
    case (_,None) => None
    case (Some(x),Some(y)) => Some(f(x,y))
  }

  /**
    * Exercise 4.3
    *
    * After reviewing "hint" of solution, guided towards a solution using flatMap or map
    */
  def map2Hinted[A,B,C](a: Option[A], b:Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap(x => b.flatMap(y => Some(f(x, y))))
  }
}
