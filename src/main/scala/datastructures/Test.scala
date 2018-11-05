package datastructures

import datastructures.Prop.{FailedCase, SuccessCount}

sealed trait Prop{
//  def check: Either[(FailedCase, SuccessCount), SuccessCount]

//  def &&(p: Prop) : Either[String, SuccessCount] =
  def check : Boolean
  def &&(p: Prop) = this.check && p.check
  def getSuccessCount : SuccessCount
}

case class Gen[A](sample: State[MyRNG, A]){

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
  }
}
object Prop {
  type SuccessCount = Int
  type FailedCase = String

  def forAll[A](a : Gen[A])(f: A => Boolean) : Prop = ???
}
class Test {

}
