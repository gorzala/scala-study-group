package ssg.chapter_two

import org.scalatest._
class MyModuleSpec extends FlatSpec with Matchers {
  "factorial" should "work correctly" in {
    val result = MyModule.factorial(4)
    result shouldBe 10000
  }

}