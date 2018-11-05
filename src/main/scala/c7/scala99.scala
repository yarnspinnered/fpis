package c7

object scala99 {
  def last[A] (l : List[A]) : Option[A] = l.foldRight(None : Option[A])((a,z) => if (z == None) Some(a) else z)

  def penultimate[A](l: List[A]) : Option[A] = {
      def go = l.foldRight((None : Option[A], 0))((a,z) => if (z._2 == 1) (Some(a), z._2 + 1) else (z._1, z._2 + 1))
      go._1
  }

  def kthLast[A](l: List[A], k : Int) : Option[A] = {
    def go = l.foldRight((None : Option[A], 0))((a,z) => if (z._2 + 1== k) (Some(a), z._2 + 1) else (z._1, z._2 + 1))
    go._1
  }

  def myLength[A](l: List[A]) : Int = l.foldRight(0)((a,z) => z + 1)

  def myReverse[A](l: List[A]) : List[A] =  l.foldLeft(Nil:List[A])((z,a) => a::z)

  def isPalindrome[A](l : List[A]) : Boolean = myReverse(l) == l

  def flatten(l: List[Any]) : List[Any] = l.foldRight(Nil:List[Any])((a,z) => a match {
    case h::t => flatten(h :: t) ++ z
    case x => x :: z
  })

  def compress[A](l : List[A]) : List[A] = l.foldRight(Nil : List[A])((a,z) => if (z == Nil || a != z.head) a::z else z)

  def pack[A](l : List[A]) : List[List[A]] = l.foldRight(Nil : List[List[A]])((a,z) => if (z==Nil || a != z.head.head ) List(a)::z else (a::z.head) :: z.tail)
  def encode[A](l : List[A]) : List[(A,Int)] = l.foldRight(Nil:List[(A,Int)])((a,z) => if (z==Nil || a != z.head._1 ) (a,1)::z else (z.head._1, z.head._2 + 1) :: z.tail)
  def main1(args: Array[String]): Unit = {
    println(last(List(1, 1, 2, 3, 5, 8)))
    println(penultimate(List(1, 1, 2, 3, 5, 8)))
    println(kthLast(List(1, 1, 2, 3, 5, 8), 1))
    println(myLength(List(1, 1, 2, 3, 5, 8)))
    println(myReverse(List(1, 1, 2, 3, 5, 8)))
    println(isPalindrome(List(1, 1, 2, 3, 5, 8)))
    println(isPalindrome(List(1, 3, 2, 3, 1)))
    println( flatten(List(List(1, 1), 2, List(3, List(5, 8)))))
println(compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
    println(pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
    println( encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
  }
}
