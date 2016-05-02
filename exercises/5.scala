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

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h,t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  /** 5.5 */
  def takeWhileFldR(p: A => Boolean): Stream[A] =
    foldRight(Empty:Stream[A])((a,b) => if (p(a)) Cons(()=>a,()=>b) else Empty)

  /** 5.6 */
  def headOptionFldR: Option[A] =
    foldRight(None:Option[A])((a,b) => Some(a))

  /** 5.7 */
  def map[B](f: A => B): Stream[B] =
    foldRight(Empty:Stream[B])((a,b) => Cons(() => f(a),() => b))
  def filter(f: A => Boolean): Stream[A] =
    foldRight(Empty:Stream[A])((x,y) => if (f(x)) Cons(()=>x,()=>y) else y)
  def append[B>:A](s:Stream[B]): Stream[B] =
    foldRight(s)((x,y) => Cons(()=>x,()=>y))
  def prepend[B>:A](s:Stream[B]): Stream[B] =
    s.foldRight(foldRight(Empty:Stream[B])((x,y) => Cons(()=>x,()=>y)))((x,y) => Cons(()=>x,()=>y))
  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Empty:Stream[B])((x,y) => y.prepend(f(x)))

  /** 5.13 */
  def mapUf[B](f: A => B): Stream[B] =
    Stream.unfold(this)((as) => as match {
      case Cons(h,t) => Some(f(h()),t())
      case _ => None
    })
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

  /** 5.8 */
  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a));

  /** 5.9 */
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n+1));

  /** 5.10 */
  def fibs: Stream[Int] = {
    def calc(a:Int, b:Int): Stream[Int] = Stream.cons(a+b, calc(b, a+b))
    cons(0,cons(1,calc(0,1)))
  }

  /** 5.11 */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = 
    f(z).map((t:(A,S)) => Stream.cons(t._1,unfold(t._2)(f))).getOrElse(Empty:Stream[A])

  /** 5.12 */
  def onesUf: Stream[Int] = unfold(1)((_) => Some((1,1)))
  def constantUf[A](a: A): Stream[A] = unfold(a)((_) => Some((a,a)))
  def fromUf(n: Int): Stream[Int] = unfold(n)((n) => Some((n,n+1)));
  def fibsUf: Stream[Int] =
    cons(0,cons(1,unfold((0,1))((t:(Int,Int)) => Some((t._1+t._2,(t._2,t._1+t._2))))))
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
    println("s.headOption: " + s.headOption);
    println("s.headOptionFldR: " + s.headOptionFldR);
    println("Empty.headOption: " + Empty.headOption);
    println("Empty.headOptionFldR: " + Empty.headOptionFldR);
    println("s.map(_+1).toList: " + s.map(_+1).toList);
    println("s.filter(_%2==0).toList: " + s.filter(_%2==0).toList);
    println("s.append(Stream(6,7,8)).toList: " + s.append(Stream(6,7,8)).toList);
    println("s.prepend(Stream(6,7,8)).toList: " + s.prepend(Stream(6,7,8)).toList);
    println("s.flatMap((a) => Stream(a-1,a,a+1)).toList: " + s.flatMap((a) => Stream(a-1,a,a+1)).toList);
    println("Stream.constant(5).take(5).toList: " + Stream.constant(5).take(5).toList);
    println("Stream.from(5).take(5).toList: " + Stream.from(5).take(5).toList);
    println("Stream.fibs.take(7).toList: " + Stream.fibs.take(7).toList);
    println("Stream.onesUf.take(5).toList: " + Stream.onesUf.take(5).toList);
    println("Stream.constantUf(5).take(5).toList: " + Stream.constantUf(5).take(5).toList);
    println("Stream.fromUf(5).take(5).toList: " + Stream.fromUf(5).take(5).toList);
    println("Stream.fibsUf.take(7).toList: " + Stream.fibsUf.take(7).toList);
    println("s.mapUf(_+1).toList: " + s.mapUf(_+1).toList);
  }
}
