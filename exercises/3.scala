sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int =
    ints match {
      case Nil => 0
      case Cons(x,xs) => x + sum(xs)
    }

  def product(ds: List[Double]): Double =
    ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x,xs) => x * product(xs)
    }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  /** 3.1
    * val x = List(1,2,3,4,5) match {
    *   case Cons(x, Cons(2, Cons(4, _))) => x :: does not match
    *   case Nil => 42 :: does not match
    *   case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y :: matches -> 1 + 2 = 3
    *   case Cons(h, t) => h + sum(t) :: matches -> 1 + sum(List(2,3,4,5) = 15
    *   case _ => 101 :: matches -> 101
    * }
    * Since the third case is the first that matches, the result will be 3
    */

  /** 3.2
    * In this solution an empty list is returned, if the input list is empty.
    * Another solution would have been to raise an exception or return an
    * object, that represents an error.
    */
  def tail[A](as: List[A]): List[A] =
    as match {
      case Nil => Nil
      case Cons(_, t:List[A]) => t
    }

  /** 3.3 */
  def setHead[A](a: A, as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(_, t:List[A]) => Cons(a,t)
  }

  /** 3.4 */
  @annotation.tailrec
  def drop[A](as: List[A], n: Int): List[A] =
    n match {
      case 1 => tail(as)
      case _ => drop(tail(as), n-1)
    }

  /** 3.5 */
  @annotation.tailrec
  def dropWhile[A](as: List[A], f: A => Boolean): List[A] =
    as match {
      case Nil => Nil
      case Cons(h, t:List[A]) => if (f(h)) dropWhile(t,f) else Cons(h,t)
    }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  /** 3.6 */
  def init[A](as: List[A]): List[A] =
    as match {
      case Nil => Nil
      case Cons(h,Nil) => Nil
      case Cons(h,t) => Cons(h,init(t))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  /** 3.7
   * As said in the book, "foldRight must must traverse all the way to the
   * end of the list (pushing frames onto the call stack as it goes) before
   * it can begin collapsing it." Because of that, placing the logic for
   * short-circuiting in the anonymous function like shown in product3 would
   * not lead to an immediately halt of the recursion. If one would call
   * product3 with a large list, it will produce a stack overflow like the
   * version product2 of this method.
   */
  def product3(ns: List[Double]) =
    foldRight(ns, 1.0)((x,y) => if (x == 0.0) 0.0 else x * y)

  /** 3.8
   * The same list is reconstructed.
   * Like said in the book, "One way of describing what foldRight does is
   * that it replaces the constructors of the list, Nil and Cons , with z
   * and f." Hence, if z and af itself are equal to Nil and Cons the same
   * list is reconstructed.
   */

  /** 3.9 */
  def length[A](as: List[A]): Int =
    foldRight(as,0)((x,y) => 1 + y)

  /** 3.10 */
  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs,f(z,x))(f)
    }

  /** 3.11 */
  def sumLeft(as: List[Int]): Int =
    foldLeft(as,0)(_ + _)
  def productLeft(as: List[Double]): Double =
    foldLeft(as,1.0)(_ * _)
  def lengthLeft[A](as: List[A]): Int =
    foldLeft(as,0)((x,y) => x + 1)

  /** 3.12 */
  def reverse[A](as: List[A]): List[A] =
    foldLeft(as,Nil:List[A])((t,h) => Cons(h,t))

  /** 3.14 */
  def appendRight[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1,a2)((h,t) => Cons(h, t))

  /** 3.15 */
  def prepend[A](a1:List[A], a2:List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,Nil) => Cons(h,a2)
      case Cons(h,t) => Cons(h,prepend(t,a2))
    }
  def concat[A](l: List[List[A]]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h,Nil) => h
      case Cons(h,t) => prepend(h,concat(t))
    }
  }

  /** 3.16 */
  def increment(s:List[Int]): List[Int] =
    foldRight(s,Nil:List[Int])((i,s) => Cons(i+1,s))
    /* does the same as:
    s match {
      case Nil => Nil
      case Cons(h,t) => Cons(h+1,increment(t))
    }
    */

  /** 3.17 */
  def toString(s:List[Double]): List[String] =
    foldRight(s,Nil:List[String])((d,s) => Cons(d.toString,s))
    /* does the same as:
    s match {
      case Nil => Nil
      case Cons(h,t) => Cons(h.toString, toString(t))
    }
    */

  /** 3.18 */
  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil:List[B])((a,b) => Cons(f(a),b))

  /** 3.19
   * To remove all odd numbers from a list enter the following in the RIPL:
   * List.filter(List(1,2,3,4,5,6,7,8,9))((x) => x % 2 == 0)
   */
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as,Nil:List[A])((x,y) => if (f(x)) Cons(x,y) else y)

  /** 3.20 */
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, Nil:List[B])((a,b) => prepend(f(a),b))

  /** 3.21 */
  def filter_flatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)((x) => if (f(x)) Cons(x,Nil) else Nil)

  /** 3.22 */
  def add(a1:List[Int], a2:List[Int]): List[Int] =
    (a1,a2) match {
      case (Nil,Nil) => Nil
      case (Nil,Cons(h,t)) => Cons(h,add(Nil,t))
      case (Cons(h,t),Nil) => Cons(h,add(t,Nil))
      case (Cons(h1,t1),Cons(h2,t2)) => Cons(h1+h2,add(t1,t2))
    }

  /** 3.23 */
  def zipWith[A](a1:List[A], a2:List[A])(f:(A,A) => A): List[A] =
    (a1,a2) match {
      case (Nil,Nil) => Nil
      case (Nil,Cons(h,t)) => Cons(h,zipWith(Nil,t)(f))
      case (Cons(h,t),Nil) => Cons(h,zipWith(t,Nil)(f))
      case (Cons(h1,t1),Cons(h2,t2)) => Cons(f(h1,h2),zipWith(t1,t2)(f))
    }
}
