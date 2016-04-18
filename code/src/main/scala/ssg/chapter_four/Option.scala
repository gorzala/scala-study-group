package ssg.chapter_four

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = {
    this match {
      case Some(a) => Some(f(a))
      case None => None
    }
  }
  def getOrElse[B >: A](ob: => B): B = {
    this match {
      case Some(a) => a
      case None => ob
    }
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    map( (a: A) => f(a).getOrElse( _ => None))
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    this match {
      case Some(a) => this
      case None => ob
    }
  }
  def filter(f: A => Boolean): Option[A] = {
    flatMap(
      (a: A) => {
        if(f(a))
          Some(a)
        else
          None
      }
    )
  }
}
case class Some[A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Test {
  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty)
      None
    else
      Some(xs.sum / xs.size)
  }
  def variance(xs: Seq[Double]): Option[Double] = {
    val sum = xs.sum
    mean(xs).flatMap{ meanValue =>
      mean(xs.map(elem => math.pow( elem - meanValue, 2)))
    }
  }
  def lift[A,B](f: A => B): (Option[A] => Option[B]) = { x: Option[A] =>
    x map f
  }

  val abs0: Option[Double] => Option[Double] = lift(math.abs)
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C) : Option[C]= {
    a.flatMap( innerA => b.map(innerB => f(innerA, innerB)))
  }

}