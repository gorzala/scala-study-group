sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  /** 5.1 */
  def toList: List[A] = this match {
    case Cons(h,t) => h() :: t().toList
    case Empty => Nil:List[A]
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}



object Test {

  def main(args: Array[String]): Unit = {
    println("Stream(1,2,3,5).toList: " + Stream(1,2,3,5).toList);
  }
}
