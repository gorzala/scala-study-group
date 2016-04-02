package ssg.chapter_three

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List{

  // Attention: not tail recursive
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }


  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // Exercise 3.2
  def tail[A](xs: List[A]): List[A] = {
    xs match {
      case Cons(h, t) => t
      case _ => xs
    }
  }

  // Exercise 3.3
  def setHead[A](h: A, xs: List[A]): List[A] = {
    Cons(h, tail(xs))
  }

  // Exercise 3.4
  def drop[A](l: List[A], n: Int): List[A] = {
    @annotation.tailrec
    def go(xs: List[A], count: Int): List[A] = {
      if (count < n) {
        go(tail(xs), count + 1)
      } else {
        xs
      }
    }
    go(l, 0)
  }

  // Exercise 3.5
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = {
    def go(l: List[A]): List[A] = {
      l match {
        case Cons(h, tail) if f(h) => go(tail)
        case _ => l
      }
    }
    go(l)
  }

  def append[A](a1: List[A], a2: List[A]) : List[A] = {
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }
  }

  // Exercise 3.6
  def init[A](l: List[A]): List[A] = {
    @annotation.tailrec
    def go(xs: List[A], build: List[A]): List[A] = {
      xs match {
        case Cons(a, Nil) => build
        case Nil => Nil
        case Cons(a, b)   =>
          val newBuild = append(build, Cons(a, Nil))
           go(b, newBuild)
      }
    }
    go(l, Nil)
  }

  // Attention: not tail recursive
  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }

  // Excercise 3.7
  // No, it could not circuit break, because foldRight always
  // iterates over the whole list. It would be necessary to include
  // the inverse Element, with regards to the


  // Excercise 3.8
  def exercise3o8() = {
     foldRight(List(1,2,3,4), Nil:List[Int])(Cons(_,_))
  }
  // It reconstructs the same List

  // Exercise 3.9
  def length[A](as: List[A]): Int = {
    foldRight(as, 0)( (elem, sum) => sum + 1 )
  }

  // Excercise 3.10
  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(first, rest) => foldLeft(rest,f(z, first))(f)
    }
  }

  // Excercise 3.11
  def newSum(as: List[Int]): Int = {
    foldLeft(as, 0)(_ + _)
  }

  def newProduct(as: List[Int]): Int = {
    foldLeft(as, 1)(_ * _)
  }

  def newLength[A](as: List[A]): Int = {
    foldLeft(as, 0)( (x, y) => x + 1 )
  }

  // Exercise 3.12
  def reverse[A](as: List[A]): List[A] = {
    def go(acc: List[A], remaining: List[A]): List[A] = {
      remaining match {
        case Nil => acc
        case Cons(a, rest) => go(append(Cons(a, Nil), acc), rest)
      }
    }
    go(acc = Nil, remaining = as)
  }

  def reverseWithFold[A](as: List[A]): List[A] = {
    foldLeft(as, Nil: List[A])( (acc: List[A], current: A)  => append(Cons(current, Nil), acc) )
  }

  // Excercise 3.13
  // TODO

  // Excercise 3.14
  def appendWithFold[A](a1: List[A], a2: List[A]): List[A] = {
    foldLeft(reverseWithFold(a1), a2)((acc, elem) => Cons(elem, acc))
  }
}