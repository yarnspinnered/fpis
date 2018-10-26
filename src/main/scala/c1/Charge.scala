package c1

case class Charge(cc : CreditCard, amt : Int) {
  def combine(other: Charge) : Charge =
    if (cc == other.cc)
      Charge(cc, amt + other.amt)
    else
      throw new Exception("Cant combine charges from different cards")
}
