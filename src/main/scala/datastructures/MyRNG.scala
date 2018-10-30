package datastructures


sealed trait MyRNG {
  def nextInt: (Int, MyRNG)

}


case class SimpleRNG(seed: Long) extends MyRNG{
  def nextInt: (Int, MyRNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n= (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object MyRNG {
  type State[S, +A] = S => (A,S)

}


case class State[S, +A](run: S => (A, S)) {
  def flatMap[B](f: A => State[S, B]) : State[S, B] =
    State((s : S) => {
      val (a, nextS) = run(s)
      f(a).run(nextS)
    })

  def map[B](f: A => B) : State[S, B] = this.flatMap(a => State.unit(f(a)))

  def map2[B, C](rb : State[S,B])(f: (A,B) => C) : State[S,C] = flatMap(raa => rb map (rbb => f(raa,rbb)))




}

object State {
  def unit[S,A](a : A) : State[S,A] = State(s  => (a, s))
  def sequence[S, A](l : List[State[S,A]]) : State[S, List[A]] =
    l.foldRight(State.unit(Nil)  : State[S, List[A]])((aa,zz) => aa.map2(zz)(_ :: _))
  def get[S] : State[S, S] = State(s => (s,s))

  def set[S](s : S) : State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int){

}

object Candy {
  def insertCoin  =
    (i : Input) => (m : Machine) => (i,m) match {
      case (Coin, Machine(_, 0, _)) => m
      case (Coin, Machine(_, candyCount, coinCount)) => Machine(false, candyCount, coinCount + 1)
      case (Turn, Machine(true, _, _ )) => m
      case (Turn, Machine(false, candyCount, coinCount)) => Machine(true, candyCount - 1, coinCount)
    }

  def simulateMachine(inputs: List[Input]) : State[Machine, (Int, Int)] =
    for {
      _ <- State.sequence (inputs map insertCoin map State.modify)
      s <- State.get
    } yield (s.coins, s.candies)


}

