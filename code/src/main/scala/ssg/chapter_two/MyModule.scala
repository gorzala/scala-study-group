package ssg.chapter_two

object MyModule {

  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n, acc * n)

    go(n, 1)
  }

  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(prev: Int, prevPrev: Int, iter: Int): Int = {
      if (iter == n)
        prev + prevPrev
      else {
        val newPrevPrev = prev
        val newPrev     = prev + prevPrev
        go(prev = newPrev, prevPrev = newPrevPrev, iter + 1)
      }
    }
    n match {
      case 0 => 0
      case 1 => 0
      case 2 => 1
      case _ => go(prev=1, prevPrev=0, iter=3)
    }
  }

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(i: Int): Boolean = {
      if (i+1 >= as.length)
        return true
      else {
        val x = as(i)
        val y = as(i+1)
        if (ordered(x,y))
          go(i+1)
        else false
      }
    }

    go(1)
  }

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    (a: A) => f(a, _)
  }

  def uncurry[A,B,C](func: A => B => C): (A, B) => C = {
    (a: A, b: B) => func(a)(b)
  }

  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))
  }

  private def formatAbs(x: Int) = {
    val msg : String = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def main(args: Array[String]): Unit = {
    val unordered = Array(1, 3, 2, 5, 6, 7)
    val ordered = Array(1, 2, 3, 4, 5, 6)
    println("unordered " + isSorted(unordered, (x: Int, y: Int) => x <= y))
    println("ordered   " + isSorted(ordered, (x: Int, y: Int) => x <= y))
    println(formatAbs(-42))
  }
}
