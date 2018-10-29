package datastructures

sealed trait MyStream[+A]{
  def headOption: Option[A] = this match {
    case sCons(h, t) => Some(h())
    case sEmpty => None
  }

  def toList: List[A] = {
    def loop(l : MyStream[A]) = l match {
      case sCons(h, t) => h() :: t().toList
      case sEmpty => Nil
    }
    loop(this)
  }

  def take(n : Int ) : MyStream[A] = this match {
    case sCons(h, t) => if (n == 0) sEmpty else MyStream.scons(h(), t().take(n - 1))
    case sEmpty => sEmpty
  }

  def drop(n : Int) : MyStream[A] = this match {
    case sCons(h, t) => if (n == 0) this else t().drop(n - 1)
    case sEmpty => sEmpty
  }

  def takeWhile(p: A => Boolean) : MyStream[A] = this match {
    case sCons(h, t) => if (p(h())) MyStream.scons(h(),t().takeWhile(p)) else sEmpty
    case sEmpty => sEmpty
  }

  def exists(p: A => Boolean) : Boolean = this match {
    case sCons(h,t) => p(h()) || t().exists(p)
    case sEmpty => false
  }

  def foldRight[B](z: => B)(f: (A, =>B) => B) : B = this match {
    case sCons(h, t) => f(h(), t().foldRight(z)(f))
    case sEmpty => z
  }

  def forAll(p: A=> Boolean):Boolean  = this match {
    case sCons(h, t) => p(h()) && t().forAll(p)
    case _ => true
  }

  def takeWhileViaFoldRight(p: A => Boolean) : MyStream[A] =
    foldRight(sEmpty : MyStream[A])((a,z) => if (p(a)) MyStream.scons(a, z) else sEmpty)

  def headOptionViaFoldRight: Option[A] =
    foldRight(None : Option[A])((a,z) =>
      Some(a))

  def map[B](f: A => B) : MyStream[B] =
    foldRight(sEmpty : MyStream[B])((a,z) => MyStream.scons({f(a)}, z))

  def append[B >: A](l : MyStream[B]) : MyStream[B] =
    this match {
      case sCons(h, t) => MyStream.scons(h(), t().append(l))
      case sEmpty => l
    }

  def filter(p: A => Boolean) : MyStream[A] =
    foldRight(sEmpty : MyStream[A])((a,z) => if (p(a)) MyStream.scons(a, z) else z)

  def flatMap[B](f: A => MyStream[B]) : MyStream[B] =
    foldRight(sEmpty : MyStream[B])((a,z) => f(a) append z)
}
case class sCons[+A](h : () => A, t: () => MyStream[A]) extends MyStream[A]
case object sEmpty extends MyStream[Nothing]

object MyStream {
  def scons[A](hd : => A, tl : => MyStream[A]) : MyStream[A] = {
    lazy val head = hd
    lazy val tail = tl
    sCons(() => head, () => tail)
  }

  def empty[A] : MyStream[A] = sEmpty

  def apply[A](as : A*): MyStream[A] = {
    if (as.isEmpty) sEmpty else scons(as.head, apply(as.tail : _*))
  }


}
