package ssg.chapter_four

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = {
    this match {
      case Some(a) => Some(f(a))
      case None => None
    }
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    this.map(f).getOrElse(None)
  }

  def getOrElse[B >: A](ob: => B): B = {
    this match {
      // How to save the Some(a) as value
      case Some(a) => a
      case None => ob
    }
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    this map (Some(_)) getOrElse(ob)
  }

  def filter(f: A => Boolean): Option[A] = {
    this flatMap { (a: A) =>
      if (f(a))
        Some(a)
      else
        None
    }
  }
}

object Option {
  def Try[A](a: => A): Option[A] = {
    try Some(a)
    catch {
      case e: Exception => None
    }
  }
  def map2[A,B,C](optA: Option[A], optB: Option[B])(f: (A,B) => C): Option[C] = {
    optA.flatMap( aa =>
      optB.map( bb =>
        f(aa,bb)
      )
    )
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]


