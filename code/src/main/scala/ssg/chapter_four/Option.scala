package ssg.chapter_four

import scala.annotation.tailrec

/**
 * Created by kirillprasalov on 08.04.16.
 */
sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case Some(a) =>
      try {
        Some(f(a))
      } catch {
        case e: Exception => None
      }
    case None => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this map f getOrElse(None)

  def getOrElse[B >: A](default : => B): B = this match {
    case Some(a) => a
    case None => default
  }

  def orElse[B >: A](ob : => Option[B]): Option[B] = (this map (Some(_))) getOrElse(ob)

  // in the book there's a different implementation using flatMap...
  def filter(f: A => Boolean): Option[A] = if (this map f getOrElse(false)) this else None

}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap((x) => b.map((y) => f(x, y)))

  // this is how sequence can be defined without traverse...
  /*
    @tailrec def sequence[A](as: List[Option[A]], acc: Option[List[A]]): Option[List[A]] = (as, acc) match {
      case (Nil, _) => acc
      case (_, None) => None
      case (a :: as, acc) => sequence(as, map2(a, acc)((x, list) => list :+ x))
    }

    def sequence[A](a: List[Option[A]]): Option[List[A]] = sequence(a, Some(Nil))
  */

  // traverse - tailrec version
  @tailrec def traverse[A, B](as: List[A], acc: Option[List[B]])(f: A => Option[B]): Option[List[B]] = (as, acc) match {
    case (Nil, _) => acc
    case (_, None) => None
    case (a :: as, acc) => traverse(as, map2(f(a), acc)((x, list) => list :+ x))(f)
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = traverse(a, Some(Nil))(f)

  // traverse - foldRight version
  def traverse1[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a.foldRight[Option[List[B]]](Some(Nil))((x, y) => map2(f(x), y)((x, list) => x :: list))


  def sequence[A](a: List[Option[A]]): Option[List[A]] = traverse(a)((x) => x)
}
