package c3

import datastructures._
object MyModule {
  def exercise31() = MyList(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case `myNil` => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h,t) => h + MyList.sum(t)
    case _ => 101
  }

  def exercise32() = MyList.tail(MyList(1,2,3,4,5))

  def exercise33() = MyList.setHead(100, MyList(1,2,3,4,5))
  def exercise34() = MyList.drop(MyList(1,2,3,4,5), 100)
  def exercise35() = MyList.dropWhile(MyList(1,2,3,4,5))((x) => x <= 3)
  def exercise36() = MyList.init(MyList(1,2,3,4,5))

  def exercise312withFold() = MyList.foldLeft(MyList(1,2,3,4,5), myNil: MyList[Int])((acc, x) => Cons(x, acc))

  def exercise312withoutFold() = MyList.reverse(MyList(1,2,3,4,5))
  def exercise313() = MyList.foldLeftViaFoldRight(MyList(1,2,3,4,5), myNil: MyList[Int])((acc, x) => Cons(x, acc))

  def exercise321() = MyList.myFlatMap(MyList(1,2,3,4))(a =>  if (a % 2 == 0) MyList(a) else myNil)

  def exercise324() = {
    val l1 = MyList(1,2,3,4)
    val l2 = MyList(2,3)
    val l3 = MyList(4,2)

    def isSubsequence[A](sup:MyList[A], sub:MyList[A]) : Boolean = {
      def helper(big:MyList[A], small:MyList[A]):Boolean = big match {

        case Cons(x,xs) => small match {
          case Cons(y,ys) => if (y==x) helper(xs,ys) else helper(xs, small)
          case myNil => helper(xs, small)
        }
        case myNil => (small == myNil)
      }
      helper(sup, sub)
    }

    isSubsequence(l1, l3)
  }
  def main1(args : Array[String]):Unit = {
    println("Ex 3.1: %s".format(exercise31()))
    println("Ex 3.2 Test tail: %s".format(exercise32()))
    println("Ex 3.3 Change first element: %s".format(exercise33()))
    println("Ex 3.4 Drop everything: %s".format(exercise34()))
    println("Ex 3.5 Drop smaller or equal to 3: %s".format(exercise35()))
    println("Ex 3.6 get everything except last: %s".format(exercise36()))
    /*
    Can foldRight terminate early?
    No, this is not possible! The reason is because _before_ we ever call our function, `f`, we evaluate its argument,
    which in the case of `foldRight` means traversing the list all the way to the end. We need _non-strict_ evaluation
      to support early termination---we discuss this in chapter 5.
    */
    println("Ex 3.8 using foldRight with constructors: %s".format(MyList.foldRight(MyList(1,2,3), myNil : MyList[Int])(Cons(_,_))))
    println("Ex 3.9 using foldRight to get length: %s".format(MyList.length(MyList(1,2))))
    println("Ex 3.10 (Writing foldleft) using foldLeft to get sum: %s".format(MyList.foldLeft(MyList(1,2, 3), 0)(_ + _)))
    println("Ex 3.11 using foldLeft to get pdt: %s".format(MyList.foldLeft(MyList(1,2, 3), 1)(_ * _)))
    println("Ex 3.11 using foldLeft to get Length: %s".format(MyList.foldLeft(MyList(1,2,3), 0)((acc, _) => acc + 1)))
    println("Ex 3.12 using foldLeft to get reverse: %s".format(exercise312withFold()))
    println("Ex 3.12 not using foldLeft to get reverse: %s".format(exercise312withoutFold()))
    println("Ex 3.13 using foldLeft to do foldRight: %s".format(exercise313()))
    println("Ex 3.14 using foldRight to do append: %s".format(MyList.appendWithFoldRight(MyList(1,2,3), MyList(4))))
    println("Ex 3.15 using appendMany: %s".format(MyList.appendMany(MyList(1,2,3), MyList(4), MyList(5,6))))
    println("Ex 3.16 using incrementAll: %s".format(MyList.incrementAll(MyList(1,2,3))))
    println("Ex 3.17 using allDoublesToString: %s".format(MyList.allDoublesToString(MyList(1.0,2.0,3.0))))
    println("Ex 3.18 using divide via myMap: %s".format(MyList.myMap(MyList(1.0,2.0,3.0))((a) => a/2)))
    println("Ex 3.19 using divide via myFilter: %s".format(MyList.myFilter(MyList(1.0,2.0,3.0))((a) => a <= 2)))
    println("Ex 3.20 using pairs via myFlatMap: %s".format(MyList.myFlatMap(MyList(1.0,2.0,3.0))((a) => MyList(a,a))))
    println("Ex 3.21 filter via flatmap: %s".format(exercise321()))
    println("Ex 3.22 add two list together: %s".format(MyList.addTwoLists(MyList(1,2,3), MyList(5, 5, 5))))
    println("Ex 3.23 add two list together via myZipWith: %s".format(MyList.myZipWith(MyList(1,2,3), MyList(5, 5, 5))(_ + _)))

    println("Trying out built-in list API")
    println("take first 2 elems of list: %s".format((1::2::3::Nil) take 2 ))
    println("takeWhile <= 3: %s".format((1::2::3::4::Nil) takeWhile (_ <= 3) ))
    println("forall even: %s".format((1::2::3::4::Nil) forall (_ % 2 == 0) ))
    println("exists even: %s".format((1::2::3::4::Nil) exists (_ % 2 == 0) ))
    println("scanLeft prefix sum: %s".format((1::2::3::4::Nil).scanLeft(0)((acc,x) => acc + x) ))
    println("Check is 2,3 subseq of 1,2,3,4: %s".format(exercise324() ))

    println(" Trying out tree ADT")
    println("Size of tree: %s".format(MyTree.size(Branch(Leaf(1), Leaf(2)))))
    println("Max of tree: %s".format(MyTree.maximum(Branch(Branch(Leaf(1), Leaf(123)), Leaf(2)))))
    println("Depth of tree: %s".format(MyTree.depth(Branch(Branch(Leaf(1), Leaf(123)), Leaf(2)))))
    println("Map of tree: %s".format(MyTree.myMap(Branch(Branch(Leaf(1), Leaf(123)), Leaf(2)))(x => x * 2)))

    println("Size of tree via myFold: %s".format(MyTree.myFold(Branch(Leaf(1), Leaf(2)))(x => 1)((l,r) => l + r + 1)))
    println("Max of tree via myFold: %s".format(MyTree.myFold(Branch(Branch(Leaf(1), Leaf(123)), Leaf(2)))(x => x)((l,r) => l max r)))
    println("Depth of tree via myFold: %s".format(MyTree.myFold(Branch(Branch(Leaf(1), Leaf(123)), Leaf(2)))(x => 1)((l,r) => 1 + (l max r))))
    println("Map of tree via myFold: %s".format(MyTree.myFold(Branch(Branch(Leaf(1), Leaf(123)), Leaf(2)))(x => Leaf(x * 2) :MyTree[Int])((l,r) => Branch(l,r))))


  }
}
