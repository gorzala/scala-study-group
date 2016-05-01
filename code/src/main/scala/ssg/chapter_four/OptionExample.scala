package ssg.chapter_four

case class Employee(name: String, department: String)

object OptionExample {
  val dataset = List(
    Employee("Marc", "Payment"),
    Employee("Jannic", "Stock")
  )

  def lockupByName(name: String): scala.Option[Employee] = {
    dataset.find( p => p.name == name)
  }

  val marcsDepartment: scala.Option[String] =
    lockupByName("Marc") map( (e: Employee) => e.department)
  val berndsDepartment: scala.Option[String] =
    lockupByName("Bernd") map( (e: Employee) => e.department)
}
