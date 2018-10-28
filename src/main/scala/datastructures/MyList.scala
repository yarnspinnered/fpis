package datastructures

sealed trait MyList[+A]
case object myNil extends MyList[Nothing]
case class Cons[+A](head :A, tail: MyList[A]) extends MyList[A]

object MyList {
  def sum(ints: MyList[Int]): Int = ints match {
    case `myNil` => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ints: MyList[Int]) : Int = ints match {
    case `myNil` => 1
    case Cons(0,xs) => 0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as : A*):MyList[A] =
    if (as.isEmpty) myNil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](xs : MyList[A]):MyList[A] = xs match
  {
    case Cons(hd, tl) => tl
    case `myNil` => myNil
  }

  def setHead[A](x: A, xs:MyList[A]) : MyList[A] = xs match
    {
    case Cons(hd, tl) => Cons(x, tl)
    case `myNil` => Cons(x, myNil)
  }

  def drop[A](l: MyList[A], n: Int) : MyList[A] =
    if (n == 0)
      l
    else
      l match {
        case Cons(x,xs) => drop(xs, n - 1)
        case `myNil` => myNil
      }

  def dropWhile[A](l:MyList[A])(p: A => Boolean):MyList[A] = l match
    {
    case `myNil` => myNil
    case Cons(x,xs) => if (p(x)) dropWhile(xs)(p) else l
  }

  def append[A](a1: MyList[A], a2: MyList[A]) : MyList[A] = a1 match {
    case `myNil` => a2
    case Cons(x,xs) => Cons(x,append(xs, a2))
  }

  def reverse[A](l: MyList[A]) : MyList[A] = {
    def helper(l : MyList[A], acc: MyList[A]) : MyList[A] = l match {
      case `myNil` => acc
      case Cons(x, xs) => helper(xs, Cons(x, acc))
    }
    helper(l, myNil)
  }
  @annotation.tailrec
  def foldLeft[A,B](as: MyList[A], z: B)(f: (B,A) => B) : B = as match {
    case `myNil` => z
    case Cons(x,xs) => foldLeft(xs, f(z, x))(f)
  }


  def foldRight[A,B](as: MyList[A], z: B)(f:(A,B) => B): B =
    as match {
      case `myNil` => z
      case Cons(x,xs) => f(x,foldRight(xs,z)(f))
    }

//  def foldLeftViaFoldRight[A,B](as: List[A], z: B)(f: (B,A) => B) : B = foldRight(reverse(as), z)((a,b) => f(b, a))

  def foldLeftViaFoldRight[A,B](l: MyList[A], z: B)(f: (B,A) => B): B =
    foldRight(l, (b:B) => b)((a,g) => b => g(f(b,a)))(z)

  def sum2(ns: MyList[Int]) = foldRight(ns, 0)(_ + _)

  def product2(ns: MyList[Int]) = foldRight(ns, 1)((x, y) => x * y)

  def length[A](ns:MyList[A]) : Int = foldRight(ns, 0)((x, y) => y + 1)

  def appendWithFoldRight[A](a1 : MyList[A], a2 : MyList[A]) : MyList[A] = foldRight(a1,a2)((a, z) => Cons(a,z))

  def incrementAll(xs: MyList[Int]): MyList[Int] = xs match {
    case `myNil` => myNil
    case Cons(a, as) => Cons(a + 1, incrementAll(as))
  }

  def myMap[A,B](xs : MyList[A])(f: (A) => B):MyList[B] = xs match {
    case `myNil` => myNil
    case Cons(a,as) => Cons(f(a), myMap(as)(f))
  }

  def myFilter[A](xs: MyList[A])(f: (A) => Boolean) : MyList[A] = xs match {
    case `myNil` => myNil
    case Cons(y,ys) => if (f(y)) Cons(y, myFilter(ys)(f)) else myFilter(ys)(f)
  }

  def myFlatMap[A,B](as:MyList[A])(f : A => MyList[B]):MyList[B] = as match {
    case `myNil` => myNil
    case Cons(y,ys) => append(f(y), myFlatMap(ys)(f))
  }
  def allDoublesToString(xs: MyList[Double]) : MyList[String] = xs match {
    case `myNil` => myNil
    case Cons(a, as) => Cons(a.toString(), allDoublesToString(as))
  }

  def addTwoLists(as: MyList[Int], bs:MyList[Int]) : MyList[Int] = as match {
    case `myNil` => myNil
    case Cons(a, aas) => bs match {
      case `myNil` => myNil
      case Cons(b, bbs) => Cons(a + b, addTwoLists(aas, bbs))
    }
  }

  def myZipWith[A](as:MyList[A], bs: MyList[A])(f : (A,A) => A) : MyList[A] = as match {
    case `myNil` => myNil
    case Cons(a, aas) => bs match {
      case `myNil` => myNil
      case Cons(b, bbs) => Cons(f(a,b), myZipWith(aas, bbs)(f))
    }
  }
  def appendMany[A](as : MyList[A]*) : MyList[A] =
    if (as.isEmpty) myNil
    else append(as.head, appendMany(as.tail: _*))
  def init[A](l: MyList[A]) : MyList[A] = l match {
    case `myNil` => myNil
    case Cons(x, `myNil`) => myNil
    case Cons(x, xs) => Cons(x,init(xs))
  }
}
