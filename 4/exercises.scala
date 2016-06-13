/** 4.1 */
sealed trait Option[+A] {

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
    case Some(a) => Some(a)
    case None => ob
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) => if (f(a)) Some(a) else None
    case None => None
  }

  /** 4.3 */
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(aa => b.map(bb => f(aa,bb)))
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  /** 4.2 */
  def variance(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else {
      var m = xs.sum / xs.length
      // xs.map(math.pow(_-m,2)).sum / xs.length
      Some(xs.map((x:Double) => math.pow(x-m,2)).sum / xs.length)
    }
    //def mean(xs: Seq[Double]): Option[Double] =
    //  if (xs.isEmpty) None
    //  else Some(xs.sum / xs.length)
    //var m = mean(xs)
    //// xs.map((x) => m.map((m:Double) => math.pow(x-m,2))).fold((x:Option[Double],y:Option[Double]) => if (x == None || y == None) None else Some(x.get + y.get)) / xs.length
    //def sum(xs: Seq[Option[Double]]): Option[Double] =
    //  if (xs.isEmpty) null else if (xs.length == 1) xs.head else 
    //xs.map((x) => m.flatMap((m:Double) => math.pow(x-m,2)))
  }

  /** 4.4 */
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    def extract[A](a: List[Option[A]]): List[A] = a match {
      case a :: as => a match { case Some(aa) => aa :: extract(as) case None => throw new Exception() }
      case Nil => Nil
    }
    Try { extract(a) }
  }

  def Try[A](a: => A): Option[A] =
    try { Some(a) }
    catch { case e: Exception => None}

  /** 4.5 */
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    Try {
      a.map(
        f(_) match {
          case Some(b) => b
          case None => throw new Exception()
        }
      )
    }
  }
}


/** 4.6 */
sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case Left(e) => Left(e)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => f(a)
    case Left(e) => Left(e)
  }

  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => Right(a)
    case Left(_) => b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this match {
    case Right(a) => b.map(bb => f(a,bb))
    case Left(e) => Left(e)
  }
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

  class HelperException[+E](left:Left[E]) extends Exception {
    def error = left
  }

  def Try[A](a: => A): Either[Exception,A] =
    try { Right(a) }
    catch { case e: Exception => Left(e) }

  /** 4.7 */
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    def extract[A](e: List[Either[E,A]]): List[A] = e match {
      case e :: es => e match {
        case Right(ee) => ee :: extract(es)
        case Left(e) => throw new HelperException(Left(e))
      }
      case Nil => Nil
    }
    try { Right(extract(es)) }
    catch { case e:HelperException[E] => e.error }
  }

  /** 4.7 */
  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    try {
      Right(
        as.map(
          f(_) match {
            case Right(b) => b
            case Left(e) => throw new HelperException(Left(e))
          }
        )
      )
    }
    catch { case e:HelperException[E] => e.error }
  }
}


object Test {

  def main(args: Array[String]): Unit = {
    println("Some(1).map(_ + 1.5): " + Some(1).map(_ + 1.5));
    // println("None[Int].map(_ + 1.5): " + None[Int].map(_ + 1.5));
    println("Some(1).flatMap(Some(_)): " + Some(1).flatMap(Some(_)));
    // println("None[Int].flatMap(_ + 1): " + None[Int].flatMap(_ + 1));
    println("Some(1).filter(_ == 1): " + Some(1).filter(_ == 1));
    println("Some(1).filter(_ == 0): " + Some(1).filter(_ == 0));
    println("None.filter(_ == 1): " + None.filter(_ == 1));
    println(Option.variance(List(1,2,3,4,5)))
    println(Option.variance(List(2,2,2,2,2,2)))
    println(Option.variance(List()))
    println(Option.sequence(List(Some(1),Some(2),Some(3),Some(4))))
    println(Option.sequence(List(Some(1),Some(2),None,Some(4))))
    println(List(1,1,1,1).map((x) => if (x == 1) Some(x) else None))
    println(List(1,1,0,1).map((x) => if (x == 1) Some(x) else None))
    println(Option.traverse(List(1,1,1,1))((x) => if (x == 1) Some(x) else None))
    println(Option.traverse(List(1,0,1,1))((x) => if (x == 1) Some(x) else None))
    println(Either.sequence(List(Right(1),Right(2),Right(3),Right(4))))
    println(Either.sequence(List(Right(1),Right(2),Left("FEHLER!"),Right(4))))
    println(Either.traverse(List(1,1,1,1))((x) => if (x == 1) Right(x) else Left("FEHLER!")))
    println(Either.traverse(List(1,0,1,1))((x) => if (x == 1) Right(x) else Left("FEHLER!")))
  }
}
