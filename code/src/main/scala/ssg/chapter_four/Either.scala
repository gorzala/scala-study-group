package ssg.chapter_four

sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] = {
    this match {
      case Left(a)  => Left(a)
      case Right(a) => Right(f(a))
    }
  }

  def flatMap[EE >: E, B](f: A => Either[EE,B]): Either[EE, B] = {
    this match {
      case Left(e)  => Left(e)
      case Right(e) => f(e)
    }
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = {
    this match {
      case Left(e) => b
      case Right(e) => Right(e)
    }
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    for {
      aa <- this
      bb <- b
    } yield(f(aa,bb))
  }

}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {


  def mean(xs: IndexedSeq[Double]): Either[String, Double] = {
    if (xs.isEmpty)
      Left("mean of an empty List!")
    else
      Right(xs.sum / xs.size)
  }

  def saveDiv(x: Int, y: Int): Either[Exception, Int] = {
    Try(x / y)
  }

  def Try[A](a: => A): Either[Exception, A] = {
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }
  }
}