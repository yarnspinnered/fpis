package c8

object MyTest {
  def main(args: Array[String]): Unit = {
    val intList = Gen.listOf(Gen.choose(0,100))
    val prop = forAll(intList)(ns => ns.reverse.reverse == ns)
    && forAll(intList)(ns => ns.headOption == ns.reverse.lastOption)

  }
}
