package ssg.chapter_four

object BadExceptions {
  def failingFnA(i: Int): Int = {
    val y: Int = throw new Exception("fail")
    try {
      val x = 42 + 5
      x + y
    } catch {
      case e: Exception => 43
    }
  }

  def failingFnB(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail")): Int)
    } catch {
      case e: Exception => 43
    }
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = {
    l.map(f)
  }

  def invokeMapWithIllegalArgumentException = {
    val list = List(1, 2, 3)
    def throwsIllegalArgumentException[A, B](a: A): B = {
      throw new IllegalArgumentException
    }
    map(list)(throwsIllegalArgumentException())
  }

  def invokeMapWithIllegalStateException = {
    val list = List(1, 2, 3)
    def throwIllegalStateException[A, B](a: A): B = {
      throw new IllegalStateException()
    }
    map(list)(throwIllegalStateException())
  }

  def mean(seq: Seq[Double]): Double = {
    seq.sum / seq.size
  }
}
