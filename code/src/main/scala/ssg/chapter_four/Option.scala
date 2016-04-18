package fpInScala.chapter04

sealed trait Option[+A] {
  //exercise 4.1
  def map[B](f : A => B) : Option[B] = this match {
    case None => None
    case Some(x) => Some(f(x))
  }
  def flatMap[B](f: A => Option[B]) : Option[B] = this match {
    case None => None
    case Some(x) => f(x)
  }
  def getOrElse[B >: A](default : => B) : B = this match {
    case None => default
    case Some(x) => x
  }
  def orElse[B >: A] (ob : => Option[B]) : Option[B] = this match {
    case None => ob
    case Some(x) => Some(x)
  }
  def filter(f : A => Boolean) : Option[A] = this match {
    case Some(x) if f(x) => Some(x)
    case _ => None
  }

}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option{
  def mean(xs : Seq[Double]) : Option[Double] =
    if (xs.isEmpty) None else Some(xs.sum / xs.length)

  // exercise 4.2
  def variance(xs : Seq[Double]) : Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  // exercise 4.3
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] ={
    a.flatMap(a => b.map(b => f(a,b)))
  }

  // exercise 4.4
  def sequence[A](a:List[Option[A]]) : Option[List[A]] =
    a.foldRight(Some(Nil):Option[List[A]])((x,acc)=>map2(x,acc)(_:: _))

  // exercise 4.5
  def traverse[A,B](a:List[A])(f : A => Option[B]) : Option[List[B]]= a match {
    case Nil => Some(Nil)
    case x :: xs => map2(f(x),traverse(xs)(f))(_::_)
  }

  def traverseViaFoldr[A,B](a:List[A])(f : A => Option[B]) : Option[List[B]] =
    a.foldRight(Some(Nil):Option[List[B]])((x,acc) => map2(f(x),acc)(_ :: _))

  def sequenceViaTraverse[A](a : List[Option[A]]) : Option[List[A]] =
    traverse(a)(x => x)
}
