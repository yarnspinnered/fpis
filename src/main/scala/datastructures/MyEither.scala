package datastructures

case class Left[+E](value:E) extends MyEither[E, Nothing]
case class Right[+A](value:A) extends MyEither[Nothing, A]

sealed trait MyEither[+E, +A]{
  def map[B](f: A => B) : MyEither[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def orElse[EE >: E, B >: A](ob : => MyEither[EE, B]) : MyEither[EE, B] = this match {
    case Left(e) => ob
    case Right(a) => Right(a)
  }

  def flatMap[EE >: E, B](f : A => MyEither[EE,B]) : MyEither[EE,B] =
    this match {
      case Left(e) => Left(e)
      case Right(x) => f(x)
    }

  def map2[EE >: E, B, C](b: MyEither[EE,B])(f: (A,B) => C):MyEither[EE,C] = {
    for {
      aa <- this
      bb <- b
    } yield f(aa,bb)
  }
}

object MyEither {
  def sequence2[E,A](es: List[MyEither[E,A]]) : MyEither[E, List[A]] =
    es.foldRight(Right(Nil) : MyEither[E, List[A]])(
      (x,z) => (x.map2(z)((xx,zz) => xx :: zz)))

  def sequence[E,A](es : List[MyEither[E,A]]) : MyEither[E, List[A]] = es match {
    case Nil => Right(Nil)
    case Left(e) :: xs => Left(e)
    case Right(a) :: xs => Right(a).map2(sequence(xs))((aa, xx) => aa :: xx)
  }

  def traverse[E,A, B](xs : List[A])(f : A => MyEither[E,B]) : MyEither[E , List[B]] =
    sequence(xs.map(f))
}
