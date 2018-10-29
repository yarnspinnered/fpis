package datastructures
import scala.{Option => _, Either => _, Some => _, _}
import datastructures.{MyList => _}

case class MySome[+A](get: A) extends MyOption[A]
case object MyNone extends MyOption[Nothing]

sealed trait MyOption[+A] {
  def map[B](f: A=>B): MyOption[B] = this match {
    case MySome(x) => MySome(f(x))
    case MyNone => MyNone
  }
  def flatMap[B](f: A=>MyOption[B]): MyOption[B] = (this map f) getOrElse(MyNone)

  def getOrElse[B >: A](default : => B) : B =  this match {
    case MySome(x) => x
    case MyNone => default
  }

  def orElse[B >: A](ob: => MyOption[B]) : MyOption[B] = (this map (MySome(_))) getOrElse ob

  def filter(f: A=> Boolean) : MyOption[A] = this flatMap(a => if (f(a)) MySome(a) else MyNone)

}


object MyOption {
  def map2[A,B,C](a: MyOption[A], b:MyOption[B])(f: (A,B) => C):MyOption[C] =
    a flatMap (ax => (b map (bx => f(ax,bx))))

  def map2ViaForComprehension[A,B,C](a: MyOption[A], b:MyOption[B])(f: (A,B) => C):MyOption[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa,bb)
  def sequence[A](as: List[MyOption[A]]) : MyOption[List[A]] =
    as.foldRight(MySome(Nil) : MyOption[List[A]])((x, z) => MyOption.map2(x,z)((newOpt, totOpt) => newOpt :: totOpt))

  def traverse[A,B](as:List[A])(f: A => MyOption[B]):MyOption[scala.List[B]] = {
    sequence (as map f)
  }
}
