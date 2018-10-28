package datastructures

sealed trait MyTree[+A]
case class Leaf[A](value : A) extends MyTree[A]
case class Branch[A](left: MyTree[A], right : MyTree[A]) extends MyTree[A]

object MyTree {
  def size[A](tr : MyTree[A]) : Int = tr match {
    case Leaf(_) => 1
    case Branch(l,r) => size(l) + size(r) + 1
  }

  def maximum(tr:MyTree[Int]) : Int = tr match {
    case Leaf(v) => v
    case Branch(l,r) => maximum(l) max maximum(r)
  }

  def depth[A](tr:MyTree[A]) : Int = tr match {
    case Leaf(_) => 1
    case Branch(l,r) => 1 + (depth(l) max depth(r))
  }

  def myMap[A, B](tr:MyTree[A])(f : A=>B) : MyTree[B] = tr match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l,r) => Branch(myMap(l)(f), myMap(r)(f))
  }

  def myFold[A,B](tr:MyTree[A])(f : A=>B)(g: (B,B) => B) : B = tr match {
    case Leaf(v) => f(v)
    case Branch(l, r) => g(myFold(l)(f)(g),myFold(r)(f)(g))
  }
}
