package c5
import datastructures.{MyStream, sEmpty}
import datastructures.MyStream._
object MyLaziness {
  def ifCondAsLazy(cond : Boolean, ifBlock : => Int, elseBlock :  => Unit) = {
    if (cond)
    {
      lazy val v = ifBlock
      v + v
    }
    else
      elseBlock
  }

  def constant[A](a:A) : MyStream[A] = {
    lazy val x : MyStream[A] = MyStream.scons(a, x)
    x
  }

  def from(n: Int) : MyStream[Int] = {
    scons(n, from(n + 1))
  }

  def fibs : MyStream[Int] =
  {
    def helper(i1 : Int, i2: Int) : MyStream[Int] =
      MyStream.scons(i1, MyStream.scons(i2, helper(i1 + i2, i2 + i1 + i2)))
    helper(0, 1)
  }




  def fromViaUnfold(n : Int) : MyStream[Int] =
  {
    MyStream.unfold(n)(seed => Some((seed, seed + 1)))
  }

  def constantViaUnfold[A](a : A) : MyStream[A] =
    unfold(a)(b => Some(b, b))

  def main1(args : Array[String]): Unit = {
    ifCondAsLazy(true, {println("if block"); 23}, println("Else Block"))
    val x = MyStream(1,2,3,4,5,6,7,8)
    val x2 = MyStream(6,7,8,9,10)
    val x3 = MyStream(10,20,30)
    val x4 = MyStream(1,2,3)
    println("Testing toList from stream: %s ".format(x.toList))
    println("Testing taking from stream: %s ".format((x take 3).toList))
    println("Testing dropping from stream: %s ".format((x drop 3).toList))
    println("Testing takeWhile from stream: %s ".format((x takeWhile (_ <= 3)).toList))
    println("Testing forAll from stream: %s ".format((x forAll (_ <= 8))))
    println("Testing takeWhileViaFoldRight from stream: %s ".format((x takeWhileViaFoldRight (_ <= 8)).toList))
    println("Testing headOptionViaFoldRight from stream: %s ".format((sEmpty headOptionViaFoldRight )))
    println("Testing map from stream: %s ".format((x map (_ * 2) )))

    println("Testing append from stream: %s".format((x append x2).toList))
    println("Testing flatMap from stream: %s".format((x flatMap (x => MyStream.scons(x, MyStream.scons(x * 10, sEmpty)))).toList))
    println("Testing filter from stream: %s".format((x filter (_ <= 3)).toList))

    lazy val ones : MyStream[Int] = constant(1)
    println("Take 5 from infinite ones: %s".format((ones take 5).toList))
    println("Check for odds from infinite ones: %s".format(ones exists (_ % 2 == 1)))
    println("Try mapping on to make twos: %s".format(ones map (_ + 1)))
    println("Try forAll odd: %s".format(ones forAll (_ != 1)))
    println("Try generating a from sequence: %s".format((from(5) take 5).toList))
    println("Try generating a fibonacci sequence: %s".format((fibs take 8).toList))

    println("Try generating a fibs sequence with unfold: %s".format((unfold(0, 1)(seed => Some(seed._1, new Tuple2(seed._2,seed._1 + seed._2)))).take(8).toList))
    println("Try generating a from sequence with unfold: %s".format(fromViaUnfold(5).take(3).toList))
    println("Try generating a constant sequence with unfold: %s".format(constantViaUnfold(5).take(3).toList))
    println("Testing mapViaUnfold from stream: %s ".format((x mapViaUnfold (_ * 2) ).take(5).toList))
    println("Testing takeViaUnfold from stream: %s ".format((ones takeViaUnfold(5)).toList))
    println("Testing takeWhileViaUnfold from stream: %s ".format((x takeWhileViaUnfold  (_ <= 3)).toList))
    println("Testing zipWithViaUnfold from stream: %s ".format((x.zipWith(x2)(_ + _)).toList))
    println("Testing zipAll from stream: %s ".format((x zipAll x3).toList))
    println("Testing startsWith from stream: %s ".format((x startsWith  x4)))
    println("Testing tails from stream: %s ".format(((x tails).map(_.toList)).toList))
    println("Testing if has subsequence from stream: %s ".format(x.tails.exists(_ startsWith x4)))
    println("Testing scanRight from stream: %s ".format(x.scanRight(0)((x,y) => {println("22323"); x + y}).take(3).toList))


  }
}
