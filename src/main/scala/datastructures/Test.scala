package datastructures

import datastructures.Gen.listOfN
import datastructures.Prop.{FailedCase, Result, SuccessCount, TestCases}


case class Gen[A](sample: State[MyRNG, A]){
  def flatMap[B](f : A => Gen[B]) : Gen[B] = Gen(sample.flatMap(a => f(a).sample))

  def listOfN2[A](g: Gen[Int]) : Gen[List[A]] = g.flatMap(a => listOfN(a,this))

}

object Gen{
  def choose(start: Int, stopExclusive: Int) : Gen[Int] = {
    Gen(State((rng : MyRNG) => {
      val (a,s) = rng.nextInt
      (start + (a % (stopExclusive - start)), s)
    }))
  }

  def unit[A](a : => A): Gen[A] = {
    Gen(State.unit(a))
  }

  def boolean:Gen[Boolean] = {
    Gen(State((rng : MyRNG) => {
      val (a,s) = rng.nextInt
      (a % 2 == 0, s)
    }))
  }

  def listOfN[A](n: Int, g: Gen[A]) : Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))

  def union[A](g1: Gen[A], g2: Gen[A]) : Gen[A] = Gen.boolean.flatMap(if (_) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
    {
      val pGen = choose(0,100).flatMap(x => unit(x.toDouble/100))
      pGen.flatMap( p => if (p < g1._2/(g1._2 + g2._2)) g1._1 else g2._1)
    }
}

case class Prop(run: (TestCases, MyRNG) => Result){
  //  def check: Either[(FailedCase, SuccessCount), SuccessCount]

  //  def &&(p: Prop) : Either[String, SuccessCount] =
  def check : Boolean = ???
  def &&(p: Prop) : Prop  = Prop {
    (n,rng) => run(n, rng) match {
      case Passed => p.run(n,rng)
      case x => x
    }
  }


  def ||(p: Prop) : Prop  = Prop {
    (n,rng) => run(n, rng) match {
      case Passed => Passed
      case Falsified(msg, _) => p.run(n,rng)
    }
  }
  def getSuccessCount : SuccessCount = ???

  def tag(msg: String) = Prop {
    (n,rng) => run(n,rng) match {
      case Falsified(e, c) => Falsified(msg + "\n" + e, c)
      case x => x
    }
  }
}

object Prop {
  type TestCases = Int
  type SuccessCount = Int
  type FailedCase = String
  type Result = Option[(FailedCase, SuccessCount)]

  def forAll[A](as : Gen[A])(f: A => Boolean) : Prop = Prop {
    (n, rng) => randomStream(as)(rng).zip(MyStream.from(0)).take(n).map {
      case (a,i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch {
        case e : Exception => Falsified(buildMsg(a,e), i)
      }

    }.find(_.isFalsified).getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: MyRNG) : MyStream[A] = MyStream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception) : String =
    s"test case: $s \n" + s" generated an exception: ${e.getMessage}\n"
}

sealed trait Result {
  def isFalsified : Boolean
}

case object Passed extends Result {
  override def isFalsified: Boolean = false
}

case class Falsified(f : FailedCase, s: SuccessCount) extends Result {
  override def isFalsified: Boolean = true
}
class Test {

}
