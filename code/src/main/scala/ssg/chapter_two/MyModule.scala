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
    def go(prev: Int, prevPrev: Int, acc: Int, iter: Int): Int = {
      if (iter == n)
        prev + prevPrev
      else {
        val newAcc = acc + prev + prevPrev
        val newPrevPrev = prev
        val newPrev     = prev + prevPrev
        go(prev = newPrev, prevPrev = newPrevPrev, acc = newAcc, iter + 1)
      }
    }
    n match {
      case 0 => 0
      case 1 => 0
      case 2 => 1
      case _ => go(prev=1, prevPrev=0, acc=0, iter=3)
    }
  }

  private def formatAbs(x: Int) = {
    val msg : String = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def main(args: Array[String]): Unit =
    println(formatAbs(-42))
}
