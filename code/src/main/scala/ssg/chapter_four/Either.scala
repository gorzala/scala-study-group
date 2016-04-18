package fpInScala.chapter04

sealed trait Either[+E, +A] {
 // exercise 4.7: implement map, flatMap, orElse and map2 for Either
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(x) => Right(f(x))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(x) => f(x)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case Right(x) => Right(x)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    this.flatMap(x => b.map(bb => f(x, bb)))
  }

  def map2For[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = for {
    x <- this
    bb <- b
  } yield f(x, bb)
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  // exercise 4.7 define sequence and traverse for Either
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    es.foldRight(Right(Nil):Either[E,List[A]])((x,acc) => for {xx <- x ; xs <- acc} yield xx :: xs)

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight(Right(Nil):Either[E,List[B]])((x,acc)=> for {xx <- f(x) ; xs <- acc} yield xx :: xs)
}