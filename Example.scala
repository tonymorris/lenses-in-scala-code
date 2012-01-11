import Lens._
import State._

object Example extends App {
  case class Employee(
    name: String
  , salary: Int
  , age: Int
  )

  object Employee {
    val name: Employee @@ String =
      Lens(
        _.name
      , (e, n) => e copy (name = n)
      )

    val salary: Employee @@ Int =
      Lens(
        _.salary
      , (e, s) => e copy (salary = s)
      )

    val age: Employee @@ Int =
      Lens(
        _.age
      , (e, a) => e copy (age = a)
      )

    val modification =
      for {
        _ <- salary += 100
        n <- name
        _ <- name := n + " Jones"
        e <- get
      } yield e
  }

  import Employee._

  val bill = Employee("Bill", 1100, 33)
  val updatedBill = modification eval bill
  println(updatedBill)
}

