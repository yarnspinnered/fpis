package c2

object MyModule {
  def abs(n:Int):Int =
    if (n < 0)
      -n
    else
      n
  @annotation.tailrec
  def factorial(n : Int, acc: Int): Int =
    if (n == 1) acc else  (factorial (n - 1, n * acc))
  private def formatAbs(x:Int) = {
    val msg = "Absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  private def formatFactorial(x : Int):String =
    "The value of factorial(%d) is %d".format(x, factorial(x, 1))

  private def formatResult(name: String, x: Int, f: Int => Int) : String=
    "The value of %s(%d) is %d".format(name, x, f(x))
  def fib(n : Int):Int = {
    @annotation.tailrec
    def oneStep(count: Int, curr: Int, next: Int): Int ={
      if (count == 1)
        curr
      else if (count == 2)
        next
      else oneStep(count - 1, next, next + curr)
    }

    oneStep(n, 0, 1)
  }

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean) = {
    def loop(n : Int) : Boolean = {
      if (n == as.length - 1)
        true
      else if (ordered(as(n),as(n + 1)))
        loop(n + 1)
      else
        false
    }
    loop(0)
  }

  def curry[A,B,C](f: (A,B) => C): A => (B => C) =
    (a : A) => (b : B) => f(a,b)

  def uncurry[A,B,C](f: A=> B => C): (A,B) => C =
    (a:A, b: B) => f(a)(b)

  def compose[A,B,C](f: B=>C, g: A => B): A => C =
    (a : A) => f(g(a))

  def main(args: Array[String]) : Unit =
    println(formatResult("abs", -42, abs))
    println(formatFactorial(5))
    for (i <- 1 until 7){
      println(s"fib $i: " + fib(i))
  }
    println("Is 1 2 3 sorted: " + isSorted(Array(1,2,3), new Function2[Int, Int, Boolean]{
      def apply(a: Int, b: Int) = a <= b
    }))

    println("Is 1 4 3 sorted: " + isSorted(Array(1,4,3), (x: Int,y:Int) => x <= y))
  println(curry(Math.max)(2)(3))

}
