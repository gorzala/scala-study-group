package fpInScala.chapter05

import Stream._

trait Stream[+A] {
  // exercise 5.1
  def toList : List[A] = this match {
    case Empty => Nil
    case Cons(h,t) => h() :: t().toList
  }

  // exercise 5.2: take and drop
  def take( n : Int) : Stream[A] = this match {
      case Cons(h,t) if n > 1 => cons(h(),t().take(n-1))
      case Cons (h,_) if n==1 => cons(h(),empty)
      case _ => empty
    }

  def drop(n : Int) : Stream[A] = this match {
    case Cons(_,t) if n > 0 => t().drop(n-1)
    case _ => this
  }

  // exercise 5.3
  def takeWhile (p : A => Boolean) : Stream[A] = this match {
    case Cons(h,t) if p(h()) => cons(h(),t().takeWhile(p))
    case _ => empty
  }

  def foldRight[B](z : => B)(f : (A,B) => B) : B = this match {
    case Cons(h,t) => f(h(),t().foldRight(z)(f))
    case Empty => z
  }

  // exercise 5.4
  def forAll(p : A => Boolean) : Boolean =
    this.foldRight(true)((x,acc) => acc && p(x))

  // exercise 5.5 takeWhile using foldr
  def takeWhileByFoldr(p : A => Boolean) : Stream[A] =
    this.foldRight(empty[A])((x,acc)=> if(p(x)) cons(x,acc) else empty)

  // TODO
  // exercise 5.6: implement headOption using foldr
  def headOption() : Option[A] =
    foldRight(None)((x,acc)=>acc)

  // exercise 5.7: implement map,filter,append and flatMap using foldr
  def map[B](f : A => B) : Stream[B] =
    foldRight(empty[B])((x,acc)=>cons(f(x),acc))

  def filter(p : A => Boolean) : Stream[A] =
    foldRight(empty[A])((x,acc)=>if(p(x)) cons(x,acc) else acc)

  def append[B >: A](xs : => Stream[B]) : Stream[B] =
    foldRight(xs)((x,acc)=>cons(x,acc))

  def flatMap[B](f : A => Stream[B]) : Stream[B] =
    foldRight(empty[B])((x,acc)=>f(x).append(acc))

  // solution to exercises 5.8 to 5.12 in associated object

  // exercise 5.13: map, take takeWhile zipWith in terms of unfold
  def mapUnfold[B](f : A => B) : Stream[B] = {
    def builder(xs: Stream[A]): Option[(B, Stream[A])] = xs match {
      case (Cons(h, t)) => Some(f(h()), t())
      case Empty => None
    }

    unfold(this)(builder)
  }

  def takeUnfold(n : Int) : Stream[A] = {
    unfold(this,n){
      case (Cons(h,t),1)          => Some((h(), (empty, 0)))
      case (Cons(h,t),n) if n > 1 => Some((h(), (t(), n-1)))
      case _                      => None
    }
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
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  // exercise 5.8
  def constant[A](a:A) : Stream[A] =
    cons(a,constant(a))

  // exercise 5.9
  def from(n : Int) : Stream[Int] =
    cons(n,from(n+1))

  // exercise 5.10
  def fibs : Stream[Int] = {
   def f(x0 : Int, x1 : Int) : Stream[Int] = {
      cons(x0,f(x1,x0+x1))
    }
    f(0,1)
  }

  // exercise 5.11
  def unfold[A,S](z : S)(f : S => Option[(A,S)]) : Stream[A] = f(z) match {
    case Some((x,zNew)) => cons(x, unfold(zNew)(f))
    case None => empty
  }


  // exercise 5.12: constant, ones, from and fibs in terms of unfold
  def constantUnfold[A](a:A) : Stream[A] =
    unfold(a)(_ => Some(a,a))

  def onesUnfold : Stream[Int] =
    unfold(1)( _ => Some(1,1))

  def fromUnfold( n : Int) : Stream[Int] =
    unfold(n)( (x) => Some (x,x+1))

  def fibsUnfold : Stream[Int] =
    unfold(0,1){ case (x,y)=> Some(x,(y,x+y)) }
}