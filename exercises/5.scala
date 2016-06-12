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
      case 0 => Empty
      case 1 => Cons(h,() => Empty)
      case _ => Cons(h,() => t().take(n-1))
    }
  }
  def drop(n:Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(_,t) => n match {
      case 0 => this
      case 1 => t()
      case _ => t().drop(n-1)
    }
  }

  /** 5.3 */
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h,t) => if (p(h())) Cons(h,() => t().takeWhile(p)) else Empty
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
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
  def takeUf(n:Int): Stream[A] =
    Stream.unfold((this, n))((z) => if (z._2 == 0) None else z._1 match {
      case Cons(h,t) => Some(h(),(t(),z._2-1))
      case _ => None
    })
  def takeWhileUf(p: A => Boolean): Stream[A] =
    Stream.unfold(this)((s) => s match {
      case Cons(h,t) => if (p(h())) Some(h(),t()) else None
      case _ => None
    })
  def zipWith[B,C](s: Stream[B])(f:(A,B) => C): Stream[C] =
    Stream.unfold((this,s))((z:(Stream[A],Stream[B])) => z match {
      case (Cons(ha,ta),Cons(hb,tb)) => Some(f(ha(),hb()),(ta(),tb()))
      case _ => None
    })
  def zipAll[B](s: Stream[B]): Stream[(Option[A],Option[B])] =
    Stream.unfold((this,s))((z:(Stream[A],Stream[B])) => z match {
      case (Cons(ha,ta),Cons(hb,tb)) => Some((Some(ha()),Some(hb())),(ta(),tb()))
      case (Cons(ha,ta),_) => Some((Some(ha()),None),(ta(),Empty))
      case (_,Cons(hb,tb)) => Some((None,Some(hb())),(Empty,tb()))
      case _ => None
    })

  /** 5.14 */
  def startsWith[A](s: Stream[A]): Boolean =
    this.zipWith(s)((a,b) => a == b).foldRight(true)((a,b) => a && b)

  /** 5.15 */
  def tails: Stream[Stream[A]] =
    Stream.unfold(this)((s) => s match {
      case Cons(h,t) => Some((Cons(h,t),t()))
      case _ => None
    })

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)
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
    println("s.take(0).toList: " + s.take(0).toList);
    println("s.drop(2).toList: " + s.drop(2).toList);
    println("s.drop(0).toList: " + s.drop(0).toList);
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
    println("s.takeUf(2).toList: " + s.takeUf(2).toList);
    println("s.takeUf(7).toList: " + s.takeUf(7).toList);
    println("s.takeWhileUf(a => { println(a); a < 3}).toList: " + s.takeWhileUf(a => { println(a); a < 3}).toList);
    println("s.zipWith(Stream(6,7,8))((a,b) => { println(a + " + " + b); a+b }).toList: " + s.zipWith(Stream(6,7,8))((a,b) => { println(a + " + " + b); a+b }).toList);
    println("s.zipAll(Stream(6,7,8)).toList: " + s.zipAll(Stream(6,7,8)).toList);
    println("s.startsWith(Stream(1)): " + s.startsWith(Stream(1)));
    println("s.startsWith(Stream(1,2)): " + s.startsWith(Stream(1,2)));
    println("s.startsWith(Stream(1,2,3)): " + s.startsWith(Stream(1,2,3)));
    println("s.startsWith(Stream(2,3)): " + s.startsWith(Stream(2,3)));
    println("s.tails.map((x) => x.toList).toList: " + s.tails.map((x) => x.toList).toList);
    println("s.hasSubsequence(Stream(1,2,3)): " + s.hasSubsequence(Stream(1,2,3)));
    println("s.hasSubsequence(Stream(5)): " + s.hasSubsequence(Stream(5)));
    println("s.hasSubsequence(Stream(2,3)): " + s.hasSubsequence(Stream(2,3)));
    println("s.hasSubsequence(Stream(4,3)): " + s.hasSubsequence(Stream(4,3)));
  }
}
