package ssg.chapter_three

import ssg.chapter_three.List.sum

object Exercise3o1 {
  def main(args: Array[String]): Unit = {
    val result = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _)))  => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }
    println (result)
  }
}
