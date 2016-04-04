object Exercises {
  /** 2.1 */
  def fib(n:Int): Int = {
    @annotation.tailrec
    def go(a:Int, b:Int, i:Int): Int = {
      if (i == n) a + b
      else go(b,a+b,i+1) 
    }
    if (n == 1) 0
    else if (n == 2) 1
    else go(1,2,3)
  }

  /** 2.2 */
  def isSorted[A](a:Array[A],ordered:(A,A)=>Boolean):Boolean = {
    @annotation.tailrec
    def loop(i:Int): Boolean = {
      if (i+1>=a.length) true
      else if (ordered(a(i),a(i+1))) loop(i+1)
      else false
    }
    loop(0)
  }

  /** 2.3 */
  def curry[A,B,C](f:(A,B) => C): A => (B => C) =
    (a:A) => ((b:B) => f(a,b))

  /** 2.4 */
  def uncurry[A,B,C](f: A => B => C): (A,B) => C =
    (a:A,b:B) => f(a)(b)

  /** 2.5 */
  def compose[A,B,C](f: B => C, g: A => B): A => C =
    (a:A) => f(g(a))
}
