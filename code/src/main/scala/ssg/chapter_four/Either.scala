package ssg.chapter_four

sealed trait Either[+E, +A]
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