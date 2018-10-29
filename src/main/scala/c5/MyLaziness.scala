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
  def main(args : Array[String]): Unit = {
    ifCondAsLazy(true, {println("if block"); 23}, println("Else Block"))
    val x = MyStream(1,2,3,4,5)
    val x2 = MyStream(6,7,8,9,10)
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


  }
}
