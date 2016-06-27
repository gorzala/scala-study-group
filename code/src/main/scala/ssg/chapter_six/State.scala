package fpInScala.chapter06

import scala.annotation.tailrec

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = Simple(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  // exercise 6.1:
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, nextRng) = rng.nextInt
    val r = if (i == Int.MinValue) 0 else scala.math.abs(i)
    (r, nextRng)
  }

  // exercise 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (i, nextRng) = nonNegativeInt(rng)
    (i / Int.MaxValue, nextRng)
  }

  // exercise 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng1) = rng.nextInt
    val (d, rng2) = double(rng1)
    ((i, d), rng2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, rng1) = double(rng)
    val (i, rng2) = rng1.nextInt
    ((d, i), rng2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)
  }

  // exercise 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count <= 0)
      (Nil, rng)
    else {
      val (i, rng1) = rng.nextInt
      val (is, rng2) = ints(count - 1)(rng1)
      (i :: is, rng2)
    }
  }

  def intsTail(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def go(count: Int)(rng: RNG, acc: List[Int]): (List[Int], RNG) = {
      if (count <= 0)
        (Nil, rng)
      else {
        val (i, nextRng) = rng.nextInt
        go(count - 1)(nextRng, i :: acc)
      }
    }
    go(count)(rng, Nil)
  }

  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  // exercise 6.5: use map to implement double in a more elegant way
  def doubleMap: Rand[Double] = {
    map(nonNegativeInt)(x => x / Int.MaxValue)
  }

  // exercise 6.6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)
    }

  // exercise 6.7 (hard)
  def sequence[A](xs: List[Rand[A]]): Rand[List[A]] =
    xs match {
      case Nil => unit(Nil)
      case f :: fs => map2(f, sequence(fs))((x, acc) => x :: acc)
    }

  // exercise 6.8: define flatmap and then use it to
  // implement nonNegativeLessThan
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (x, rng1) = f(rng)
    g(x)(rng1)
  }
  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt){ i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
  }

  // exercise 6.9: Reimplement map and map2 in terms of flatMap
  def mapFM[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2FM[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => flatMap(rb)(b => unit(f(a,b))))
}

// exercise 6.10: generalization to State
case class State[S, +A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))

  def map2[B,C](s: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => s.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, nextS) = run(s)
    f(a).run(nextS)
  })
}

object State {

  def unit[S,A](a : A) : State[S,A] =
    State(s => (a,s))

  def sequence[A,S](xs : List[State[S,A]]) : State[S,List[A]] =
    xs match {
      case Nil => unit(Nil)
      case f :: fs => f.map2(sequence(fs))((x, acc) => x :: acc)
    }
}
