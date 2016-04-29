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

  /** 5.2 */
  def take(n:Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h,t) => n match {
      case 1 => Cons(h,() => Empty)
      case _ => Cons(h,() => t().take(n-1))
    }
  }
  def drop(n:Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(_,t) => n match {
      case 1 => t()
      case _ => t().drop(n-1)
    }
  }

  /** 5.3 */
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h,t) => if (p(h())) Cons(h,() => t().takeWhile(p)) else Empty
  }

  /** 5.4 */
  def forAll(p: A => Boolean): Boolean = this match {
    case Empty => true
    case Cons(h,t) => if (p(h())) t().forAll(p) else false
  }

  /** 5.5 */
  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h,t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }
  def takeWhileFldR(p: A => Boolean): Stream[A] =
    foldRight(Empty:Stream[A])((a,b) => if (p(a)) Cons(()=>a,()=>b) else Empty)
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
    var s = Stream(1,2,3,4,5)
    println("var s = Stream(1,2,3,4,5);");
    println("s.toList: " + s.toList);
    println("s.take(2).toList: " + s.take(2).toList);
    println("s.drop(2).toList: " + s.drop(2).toList);
    println("s.takeWhile(a => { println(a); a < 3}).toList: " + s.takeWhile(a => { println(a); a < 3}).toList);
    println("s.forAll(a => { println(a); a < 3 }): " + s.forAll(a => { println(a); a < 3 }));
    println("s.takeWhileFldR(a => { println(a); a < 3}).toList: " + s.takeWhileFldR(a => { println(a); a < 3}).toList);
  }
}
