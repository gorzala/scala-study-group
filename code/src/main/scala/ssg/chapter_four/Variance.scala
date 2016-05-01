package ssg.chapter_four

object Variance {

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(
      m => mean(
        xs.map(x => math.pow(x -m, 2))
      )
    )
  }

  def mean(xs: Seq[Double]): Option[Double] = {
    xs match {
      case Nil => None
      case _ => Some(xs.sum / xs.size)
    }
  }
}
