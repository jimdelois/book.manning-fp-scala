package ch4

sealed trait Option[+A] {
  /**
    * Exercise 4.1
    *
    * Implement all of the functions [map, flatMap, getOrElse, orElse, filter]
    * on Option. As you implement each function, try to think about what it means
    * and in what situations you’d use it. We’ll explore when to use each of
    * these functions next.
    */
  def map[B](f: A => B): Option[B] = this match {
    case Some(result) => Some(f(result))
    case None => None
  }

  /**
    * Exercise 4.1
    */
  def flatMap[B](f: A => Option[B]): Option[B] = {
    this.map(f).getOrElse(None)
  }

  /**
    * Exercise 4.1
    */
  def getOrElse[B >: A](default: => B): B = this match {
    case Some(result) => result
    case None => default
  }

  /**
    * Exercise 4.1
    */
  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    this.map(Some(_)).getOrElse(ob)
  }

  /**
    * Exercise 4.1
    */
  def filter(f: A => Boolean): Option[A] = {
    this.flatMap(a => if (f(a)) this else None)
  }
}

case object None extends Option[Nothing]

case class Some[+A](get: A) extends Option[A]

