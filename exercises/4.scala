/** 4.1 */
sealed trait Option[+A] {

  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(a) => f(a)
    case None => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(b) => b
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case Some(b) => Some(b)
    case None => ob
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) => if (f(a)) Some(a) else None
    case None => None
  }
}
