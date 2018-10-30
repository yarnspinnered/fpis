package c6

import datastructures._

object MyFunctionalState {
  type Rand[+A] = MyRNG => (A, MyRNG)

  val intAsRand : Rand[Int] = _.nextInt

  def unit[A](a : A) : Rand[A] = (rng) => (a, rng)

  def map[A,B](s: Rand[A])(f : A => B) : Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f : (A,B) => C) : Rand[C] = rng =>
  {
    val (a, ra2) = ra(rng)
    val (b, rb2) = rb(ra2)
    (f(a,b), rb2)
  }

  def flatMap[A,B](f:Rand[A])(g: A => Rand[B]) : Rand[B] =
    s =>
      {
        val (a, nextS) = f(s)
        g(a)(s)
      }

  def mapViaFlatMap[A,B](s: Rand[A])(f : A => B) : Rand[B] =
    flatMap(s)(seed => unit(f(seed)))

  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f : (A,B) => C) : Rand[C] =
    flatMap(ra)(raa => map(rb)(rbb => f(raa, rbb)))

  def nonNegativeLessThan(n : Int) : Rand[Int] =
    flatMap(nonNegativeInt){ i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
  def sequence[A](fs: List[Rand[A]]) : Rand[List[A]] =
    fs.foldRight(unit(Nil : List[A]))((a,z) => map2(a, z)((aa,zz) => aa::zz))

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i%2)

  def doubleViaMap : Rand[Double] = map(nonNegativeInt)(i => i.toDouble/(Integer.MAX_VALUE + 1))

  def nonNegativeInt(rng: MyRNG) : (Int, MyRNG) = rng.nextInt match {
    case (x, nextRNG) if x > Integer.MAX_VALUE => (Integer.MAX_VALUE, nextRNG)
    case (x, nextRNG) if x == Integer.MIN_VALUE => (Integer.MAX_VALUE, nextRNG)
    case (x, nextRNG) if x < 0 => (-x, nextRNG)
    case (x, nextRNG) => (x, nextRNG)
  }

  def doubleRNG(rng: MyRNG) : (Double, MyRNG) = rng.nextInt match {
    case (x, nextRNG) => if (x < 0) (x.toDouble/Integer.MIN_VALUE, nextRNG) else (x.toDouble/Integer.MAX_VALUE, nextRNG)
  }

  def intDoubleRNG(rng: MyRNG) : ((Int, Double), MyRNG) =
    {
      val (i, nextRNG) = nonNegativeInt(rng)
      val (d, nextRNG2) = doubleRNG(rng)
      ((i,d),nextRNG)
    }

  def doubleIntRNG(rng: MyRNG) : ((Double, Int), MyRNG) =
    intDoubleRNG(rng) match {
      case ((i,d), nextRNG) => ((d,i), nextRNG)
    }

  def double3RNG(rng: MyRNG) : ((Double, Double, Double), MyRNG) = {
    val (d1, rng1) = doubleRNG(rng)
    val (d2, rng2) = doubleRNG(rng1)
    val (d3, rng3) = doubleRNG(rng2)
    ((d1, d2,d3), rng3)
  }

  def randomInts(count: Int)(rng: MyRNG) : (List[Int], MyRNG) =
  count match {
    case 0 => (Nil, rng)
    case n => {
      val (i, nextRNG) = rng.nextInt
      val (l, latestRNG) = randomInts(n - 1)(nextRNG)
      (i :: randomInts(n - 1)(nextRNG)._1, latestRNG)
    }
  }

def main(args : Array[String]) : Unit = {
  val simple = SimpleRNG(42);
  val (n1, rng2) = simple.nextInt;
  val (n2, rng3) = rng2.nextInt;
  println("First val %d, second val: %d".format(n1, n2) )

  println("My nonneg Ints: %s %s".format(nonNegativeInt(simple), nonNegativeInt(nonNegativeInt(simple)._2)))
  println("My double: %s %s".format(doubleRNG(simple), doubleRNG(doubleRNG(simple)._2)))
  println("My intDouble: %s".format(intDoubleRNG(simple)))
  println("My doubleInt: %s".format(doubleIntRNG(simple)))
  println("My list of ints: %s".format(randomInts(3)(simple)))
  println("My ints via sequence: %s".format(sequence(List.fill(5)(intAsRand))(simple)))
  println("My candy machine: %s".format(Candy.simulateMachine(Turn::Coin::Coin::Coin::Nil).run(Machine(false, 1, 0))))



}
}
