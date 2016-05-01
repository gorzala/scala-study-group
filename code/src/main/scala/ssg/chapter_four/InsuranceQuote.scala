package ssg.chapter_four


object InsuranceQuote {

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = {
    (age * numberOfSpeedingTickets).toDouble
  }

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
    val optAge = Option.Try(age.toInt)
    val optTickets = Option.Try(numberOfSpeedingTickets.toInt)

    Option.map2(optAge, optTickets)(insuranceRateQuote)
  }

  def parseWithEither(age: String, numberOfTickets: String): Either[Exception, Double] = {
    val optAge = Either.Try(age.toInt)
    val optTickets = Either.Try(numberOfTickets.toInt)

    optAge.map2(optTickets)(insuranceRateQuote)
  }
}
