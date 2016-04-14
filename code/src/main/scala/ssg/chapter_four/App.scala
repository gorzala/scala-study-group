package ssg.chapter_four

/**
 * Created by kirillprasalov on 07.04.16.
 */
object App {

  def main(args: Array[String]): Unit = {
    println("Ex. 4.1")

    println("map of Some: " + Some(4).map(_ * 2))
    println("map of None: " + None.asInstanceOf[Option[Int]].map(_ * 2))
    println("map of Some with exception: " + Some(4).map(_ / 0))
    println()

    println("flatmap of Some: " + Some(4).flatMap(intToOption))
    println("flatmap of None: " + None.asInstanceOf[Option[Int]].flatMap(intToOption))
    println("flatmap of Some with exception: " + Some(0).flatMap(intToOption))
    println()

    println("getOrElse of Some: " + Some(4).getOrElse(0))
    println("getOrElse of None: " + None.asInstanceOf[Option[Int]].getOrElse(0))
    println()

    println("orElse of Some: " + Some(4).orElse(Some(0)))
    println("orElse of None: " + None.asInstanceOf[Option[Int]].orElse(Some(0)))
    println()

    println("filter of Some holding a predicate: " + Some(4).filter(isNotZero))
    println("filter of Some failing a predicate: " + Some(0).filter(isNotZero))
    println("filter of None: " + None.asInstanceOf[Option[Int]].filter(isNotZero))
    println()

    println("**********")
    println()

    println("Ex. 4.3")

    println("variance: " + variance(List(0.1, 0.2, 0.3)))
    println("variance of an empty list: " + variance(Nil))

    println()
    println("**********")
    println()

    println("Ex. 4.4")

    println("sequence: " + Option.sequence(List(Some(1), Some(2), Some(3))))
    println("sequence with None: " + Option.sequence(List(Some(1), None, Some(3))))

    println()
    println("**********")
    println()

    println("Ex. 4.5")

    println("traverse: " + Option.traverse1(List(1, 2, 3))(divideByZero))
    println("traverse with None: " + Option.traverse1(List(1, 0, 3))(divideByZero))

    println()
    println("**********")
    println()

    println("Ex. 4.6")

    val right: Either[Exception, Int] = Right(5)
    val left: Either[Exception, Int] = Left(new Exception)
    println("map of right: " + right.map(_ * 2))
    println("map of left: " + left.map(_ * 2))

    println("flatMap of right: " + right.flatMap((x) => Right(x * 2)))
    println("flatMap of left: " + left.flatMap((x) => Right(x * 2)))

    println("orElse of right: " + right.orElse(Right(0)))
    println("orElse of left: " + left.orElse(Right(0)))

    println("map2 of right: " + right.map2(Right(5))(_ + _))
    println("map2 of right and left: " + right.map2(left)(_ + _))
    println("map2 of left: " + left.map2(Right(5))(_ + _))

    println()
    println("**********")
    println()

    println("Ex. 4.7")
    println("traverse: " + Either.traverse(List(1, 2, 3))(divideByZero1))
    println("traverse with Left: " + Either.traverse(List(1, 0, 3))(divideByZero1))

    println("sequence: " + Either.sequence(List(Right(1), Right(2), Right(3))))
    println("sequence with Left: " + Either.sequence(List(Right(1), Left(new NullPointerException), Right(3))))

  }

  def intToOption(x: Int) = if (x == 0) None else Some(x)
  def isNotZero(x: Int) = x != 0
  def divideByZero(x: Int) = if (x == 0) None else Some(10 / x)
  def divideByZero1(x: Int) = if (x == 0) Left(new IllegalArgumentException) else Right(10 / x)

  def mean(ys: Seq[Double]) : Option[Double] = ys match {
    case Nil => None
    case _ => Some(ys.sum / ys.size)
  }

  // in the book there's a shorter version, the first map seems to be redundant here
  // one could do flatMap using mean in the very beginning instead...
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs)
      .map(
        (m) => xs.map(
          (x) => math.pow(x - m, 2)
        )
      )
      .flatMap(mean)
}
