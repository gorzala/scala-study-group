package ssg.chapter_four

import scala.annotation.tailrec

/**
 * Created by kirillprasalov on 08.04.16.
 */
sealed trait Either[+E, +A] {

  // how do we handle a possible exception when calling f(a)??
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  // in the book it's slightly different...
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this map f match {
    case Left(e) => Left(e)
    case Right(a) => a
  }

  def orElse[EE >: E, B >: A](b : => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => b
    case _ => this
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this flatMap((a1) => b map((b1) => f(a1, b1)))

  def map2_using_for[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      one <- this
      another <- b
    } yield f(one, another)
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

  // also could be done using foldRight...
  @tailrec def traverse[A, B, E](as: List[A], res: Either[E, List[B]])(f: A => Either[E, B]): Either[E, List[B]] = (as, res) match {
    case (Nil, _) => res
    case (_, Left(e)) => Left(e)
    case (a :: as, res) => traverse(as, res.map2(f(a))((list, newVal) => list :+ newVal))(f)
  }

  def traverse[A, B, E](a: List[A])(f: A => Either[E, B]): Either[E, List[B]] = traverse(a, Right(Nil))(f)

  def sequence[A, E](a: List[Either[E, A]]) = traverse(a)((a) => a)
}