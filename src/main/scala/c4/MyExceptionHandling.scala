package c4
import datastructures.{None, Option, Some, MyEither}
import scala.{Option => _, Either => _, Some => _, None => _, _}

case class Employee(name: String, department: String){
  def lookupByName(name:String): Option[Employee] = if (name == this.name) Some(this) else None
}

object MyExceptionHandling {
  def mean(xs:Seq[Double]): Option[Double] = xs match {
    case Nil => None
    case x :: xxs => Some((xs.foldRight(0.0)((x,z) => x + z))/xs.length)
  }

  def variance(xs: Seq[Double]) : Option[Double] =
    {
        mean(xs).map(m => xs.map(x  => math.pow(x - m, 2))).flatMap(xs => mean(xs))
    }

  def Try[A](a : => A) : Option[A] =
    try Some(a)
    catch {case e: Exception => None}

  def parseInsuranceRateQuote(age: String, numberOfSpeedTickets : String) : Option[Int] = {
    val optAge : Option[Int] = Try(age.toInt)
    val optTickets : Option[Int] = Try(numberOfSpeedTickets.toInt)
    optAge.flatMap(x => optTickets.map(y => x + y))
  }

  def ex43(age: String, numberOfSpeedTickets : String) : Option[Int] = {
    val optAge : Option[Int] = Try(age.toInt)
    val optTickets : Option[Int] = Try(numberOfSpeedTickets.toInt)
    Option.map2(optAge, optTickets)(_+_)
  }

  def safeDiv(x: Int, y: Int):Either[Exception, Int] =
    try Right(x/y)
    catch {
      case e: Exception => Left(e)
    }

  def tryEither[A](a : => A):Either[Exception, A] =
    try Right(a)
    catch {
      case e : Exception => Left(e)
    }
  def main1(args: Array[String]): Unit = {
    val joe = Employee("joe", "joesDepartment")
    println("Joe's dept is : %s ".format(joe.lookupByName("joe").map(_.department)))
    println("Johns's dept is : %s".format(joe.lookupByName("john").map(_.department)))
    println("Johns's dept is : %s".format(joe.lookupByName("john").map(_.department).getOrElse("Default dept")))
    println("Variance of 1::2::3: %s".format(variance(1.0::2.0::3.0::Nil)))
    println("Mean of 1::2::3: %s".format(mean(1.0::2.0::3.0::Nil)))

    println(joe.lookupByName("joe")
      .map(_.department)
      .filter(_ == "joesDepartment")
      .getOrElse("Default Dept"))

    println("Insurance rate quote parse: " + parseInsuranceRateQuote("12", "23"))
    println("ex 4.3 Using Map2: " + ex43("12", "23"))
    println("ex 4.4 Using sequence: " + Option.sequence(Some(1)::Some(2)::Some(3) :: Nil))
    println("ex 4.5 Using traverse: " + Option.traverse(1::2::3 :: Nil)((x : Int) => Try(x/(x-0))))

    println("ex 4.7 Using either sequence when failure present: " + MyEither.sequence(datastructures.Right(1)::datastructures.Right(2)::datastructures.Left("Random failer") :: Nil))
    println("ex 4.7 Using either sequence when success: " + MyEither.sequence(datastructures.Right(1)::datastructures.Right(2):: Nil))
    println("ex 4.7 Using either traverse: " + MyEither.traverse(1::2::3 :: Nil)((x : Int) => datastructures.Right(x * 2)))
  }
}
