package fpInScala.chapter03

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  // exercise 3.1 what is the result of the following pattern match?
  // - third pattern is matched: 3
  val xx = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))

  // exercise 3.2
  def tail[A](xs: List[A]): List[A] = xs match {
    case Nil => Nil // usually this case throws an exception
    case Cons(_, ys) => ys
  }

  // exercise 3.3
  def setHead[A](x: A, xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case Cons(_, ys) => Cons(x, ys)
  }

  // exercise 3.4
  def drop[A](n: Int, list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(x, xs) => if (n <= 0) Cons(x, xs) else drop(n - 1, xs)
  }

  // exercise 3.5
  def dropWhile[A](list: List[A], p: A => Boolean): List[A] = list match {
    case Nil => Nil
    case Cons(x, xs) => if (p(x)) dropWhile(xs, p) else Cons(x, xs)
  }

  // exercise 3.6
  def init[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  /* exercise 3.7: can product (implemented by foldr) immediately halt the
  recursion and return 0.0 if it encounters a 0.0?
  - No, since functions in scala are strict by default.

  exercise 3.8 what happens when you pass Nil and Cons to foldRight?
  - It returns the input list.
  */

  // exercise 3.9
  def length[A](xs: List[A]): Int = foldRight(xs, 0)((_, acc) => 1 + acc)

  //  exercise 3.10
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  // exercise 3.11 (sumL,productL,lengthL)
  def sumL(xs: List[Int]): Int = foldLeft(xs, 0)(_ + _)

  def productL(xs: List[Int]): Int = foldLeft(xs, 1)(_ * _)

  def lengthL(xs: List[Int]): Int = foldLeft(xs, 0)((acc, _) => 1 + acc)

  //exercise 3.12
  def reverse[A](list: List[A]): List[A] =
    foldLeft(list, Nil: List[A])((xs, x) => Cons(x, xs))

  // exercise 3.13: foldLeft in terms of foldRight and the other way around
  def foldlViaFoldr[A,B](as: List[A], z :B)(f : (B,A)=> B) : B =
    foldRight(as,(x:B) => x)((x,acc)=> y => acc(f(y,x)))(z)

  def foldrViaFoldl[A,B](as: List[A], z :B)(f : (A,B)=> B) : B =
    foldLeft(as,(x:B) => x)((acc,x)=> y => acc(f(x,y)))(z)

  // exercise 3.14
  def append[A](list1: List[A], list2: List[A]): List[A] =
    foldRight(list1, list2)((x, acc) => Cons(x, acc))

  // exercise 3.15: concat with linear runtime in total length
  def concat[A](xss: List[List[A]]): List[A] = xss match {
    case Nil => Nil
    case Cons(Nil, yss) => concat(yss)
    case Cons(Cons(y, ys), yss) => Cons(y, concat(Cons(ys, yss)))
  }

  // exercise 3.16: map(+1)
  def addOne(list: List[Int]): List[Int] = list match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x + 1, addOne(xs))
  }

  // exercise 3.17: map(*2)
  def double(list: List[Double]): List[Double] = list match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x * 2, double(xs))
  }

  // exercise 3.18: map
  def map[A, B](list: List[A], f: A => B): List[B] = list match {
    case Nil => Nil
    case Cons(x, xs) => Cons(f(x), map(xs, f))
  }

  // exercise 3.19: define filter and remove odd numbers from a list using filter
  def filter[A](list: List[A], p: A => Boolean): List[A] = list match {
    case Nil => Nil
    case Cons(x, xs) => if (p(x)) Cons(x, filter(xs, p)) else filter(xs, p)
  }

  def removeOdds(list: List[Int]): List[Int] = filter[Int](list, x => x % 2 == 0)

  // exercise 3.20: concatMap
  def flatMap[A, B](list: List[A], f: A => List[B]) = concat(map(list, f))

  // exercise 3.21: filter using flatMap
  def filterFM[A](list: List[A], p: A => Boolean): List[A] =
    flatMap(list, (x: A) => if (p(x)) Cons(x, Nil) else Nil)

  // exercise 3.22: zipWith (+)
  def combine(list1: List[Int], list2: List[Int]): List[Int] = (list1, list2) match {
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, combine(xs, ys))
    case _ => Nil
  }

  // exercise 3.23: zipWith
  def zipWith[A, B, C](list1: List[A], list2: List[B], f: (A, B) => C): List[C] = (list1, list2) match {
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys, f))
    case _ => Nil
  }

  // exercise 3.24
  def hasSubsequence[A](text: List[A], pattern: List[A]): Boolean = (text,pattern) match {
    case (_, Nil) => true
    case (Nil,Cons(_, _)) => false
    case (Cons(t, ts), Cons(p, ps)) => (p==t && hasSubsequence(ts,ps)) ||  hasSubsequence(ts, pattern)
  }
}

