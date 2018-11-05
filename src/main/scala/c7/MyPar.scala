package c7

import java.util.concurrent.Executors

import datastructures.Par

object MyPar {
  def main(args: Array[String]): Unit = {
    val a = Par.lazyUnit(42 + 1)
    val S = Executors.newFixedThreadPool(2)
    println(Par.equal(S)(a, Par.fork(a)))
  }
}
