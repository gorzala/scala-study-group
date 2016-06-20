trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {
  /** 6.1 */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    var tmp = rng.nextInt
    while (tmp._1 == Int.MinValue)
      tmp = tmp._2.nextInt
    (if(tmp._1 < 0) -tmp._1 else tmp._1, tmp._2)
  }

  /** 6.2 */
  def double(rng: RNG): (Double, RNG) = {
    var tmp = nonNegativeInt(rng)
    while (tmp._1 == Int.MaxValue)
      tmp = nonNegativeInt(tmp._2)
    (tmp._1.toDouble / Int.MaxValue.toDouble , tmp._2)
  }
}



object Test {
  def main(args: Array[String]): Unit = {
    val rng = new SimpleRNG(1l)

    println("\nNon-Negative Integers")
    var tmp = RNG.nonNegativeInt(rng)
    println(tmp._1)
    tmp = RNG.nonNegativeInt(tmp._2)
    println(tmp._1)
    tmp = RNG.nonNegativeInt(tmp._2)
    println(tmp._1)
    tmp = RNG.nonNegativeInt(tmp._2)
    println(tmp._1)
    tmp = RNG.nonNegativeInt(tmp._2)
    println(tmp._1)
    tmp = RNG.nonNegativeInt(tmp._2)
    println(tmp._1)
    tmp = RNG.nonNegativeInt(tmp._2)
    println(tmp._1)
    tmp = RNG.nonNegativeInt(tmp._2)
    println(tmp._1)
    tmp = RNG.nonNegativeInt(tmp._2)
    println(tmp._1)
    tmp = RNG.nonNegativeInt(tmp._2)
    println(tmp._1)

    println("\nProbabilities less then 1.0")
    var tmp2 = RNG.double(rng)
    println(tmp2._1)
    tmp2 = RNG.double(tmp2._2)
    println(tmp2._1)
    tmp2 = RNG.double(tmp2._2)
    println(tmp2._1)
    tmp2 = RNG.double(tmp2._2)
    println(tmp2._1)
    tmp2 = RNG.double(tmp2._2)
    println(tmp2._1)
    tmp2 = RNG.double(tmp2._2)
    println(tmp2._1)
    tmp2 = RNG.double(tmp2._2)
    println(tmp2._1)
    tmp2 = RNG.double(tmp2._2)
    println(tmp2._1)
    tmp2 = RNG.double(tmp2._2)
    println(tmp2._1)
    tmp2 = RNG.double(tmp2._2)
    println(tmp2._1)

    println()
  }
}
