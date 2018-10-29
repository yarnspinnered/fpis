package datastructures

sealed trait MyStream[+A]{
  def headOption: MyOption[A] = this match {
    case sCons(h, t) => MySome(h())
    case sEmpty => MyNone
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

  def headOptionViaFoldRight: MyOption[A] =
    foldRight(MyNone : MyOption[A])((a, z) =>
      MySome(a))

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

  def mapViaUnfold[B](f: A=> B) : MyStream[B] =
    MyStream.unfold(this)(seed => seed match {
      case sCons(h, t) => Some((f(h()), t()))
      case sEmpty => None
    })

  def takeViaUnfold(n : Int) : MyStream[A] =
    MyStream.unfold((this, n))(seed => seed match {
      case (oldStream, n) =>
        oldStream match {
          case sCons(h, t) => if (n == 1) Some(h(), (sEmpty, n - 1)) else Some(h(), (t(), n - 1))
          case sEmpty => None
      }
    })

  def takeWhileViaUnfold(p: A => Boolean) : MyStream[A] =
    MyStream.unfold(this){
      case sCons(h, t) if (p(h()))  => Some(h(), t())
      case sEmpty => None
    }

  def zipWith[B](s : MyStream[B])(f : (A,B) => B) : MyStream[B] =
    MyStream.unfold((this, s)){
      case (sCons(h, t), sCons(h2, t2)) => Some(f(h(), h2()), (t(), t2()))
      case _ => None
    }

  def zipAll[B](s2: MyStream[B]) : MyStream[(Option[A], Option[B])] =
    MyStream.unfold((this, s2)){
      case (sCons(h, t), sCons(h2, t2)) => Some(((Some(h()), Some(h2())),  (t(), t2())))
      case (sCons(h, t), `sEmpty`) => Some(((Some(h()), None),  (t(), sEmpty)))
      case (`sEmpty`, sCons(h2, t2)) => Some(((None, Some(h2())),  (sEmpty, t2())))
      case (`sEmpty`, `sEmpty`) => None
    }

  def startsWith[A](s2: MyStream[A]) : Boolean = (zipAll(s2)).forAll{
    case (Some(a), Some(b)) => a == b
    case (Some(a), None) => true
    case _ => false
  }

  def tails: MyStream[MyStream[A]] =
    MyStream.unfold(this){
      case sCons(h, t) => Some( MyStream.scons(h(), t()), t())
      case sEmpty => None
    }

  def scanRight[B](z : B)(f : (A, =>B) => B) : MyStream[B] =
    foldRight((z, MyStream(z))){
      case (a, (zz, acc)) => {
        lazy val interm = f(a,zz)
        (interm, MyStream.scons(interm, acc))
      }

    }._2

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

  def unfold[A,S](z:S)(f: S => Option[(A,S)]) : MyStream[A] =
    f(z) match {
      case Some((a,new_z)) => MyStream.scons(a, unfold(new_z)(f))
      case None => sEmpty
    }
}
