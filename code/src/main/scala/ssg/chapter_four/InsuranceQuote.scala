package ssg.chapter_four

import Option.Try
import Option.map2

object InsuranceQuote {
  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = {
    (age * numberOfSpeedingTickets).toDouble
  }

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
    val optAge = Try(age.toInt)
    val optTickets = Try(numberOfSpeedingTickets.toInt)

    map2(optAge, optTickets)(insuranceRateQuote)
  }
}
