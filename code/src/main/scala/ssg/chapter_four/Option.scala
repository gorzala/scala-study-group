package ssg.chapter_four

sealed trait Option[+A]
case class Some[A](get: A) extends Option[A]
case class object none extends Option[Nothing]