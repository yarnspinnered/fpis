package c1

class CreditCard(var bal: Int) {

  def charge(amount : Int): Unit ={
    bal = bal - amount;
  }

}
