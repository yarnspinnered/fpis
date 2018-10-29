package datastructures
import scala.{Option => _, Either => _, Some => _, _}
import datastructures.{MyList => _}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

sealed trait Option[+A] {
  def map[B](f: A=>B): Option[B] = this match {
    case Some(x) => Some(f(x))
    case None => None
  }
  def flatMap[B](f: A=>Option[B]): Option[B] = (this map f) getOrElse(None)

  def getOrElse[B >: A](default : => B) : B =  this match {
    case Some(x) => x
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]) : Option[B] = (this map (Some(_))) getOrElse ob

  def filter(f: A=> Boolean) : Option[A] = this flatMap(a => if (f(a)) Some(a) else None)

}


object Option {
  def map2[A,B,C](a: Option[A], b:Option[B])(f: (A,B) => C):Option[C] =
    a flatMap (ax => (b map (bx => f(ax,bx))))

  def map2ViaForComprehension[A,B,C](a: Option[A], b:Option[B])(f: (A,B) => C):Option[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa,bb)
  def sequence[A](as: List[Option[A]]) : Option[List[A]] =
    as.foldRight(Some(Nil) : Option[List[A]])((x, z) => Option.map2(x,z)((newOpt, totOpt) => newOpt :: totOpt))

  def traverse[A,B](as:List[A])(f: A => Option[B]):Option[scala.List[B]] = {
    sequence (as map f)
  }
}
