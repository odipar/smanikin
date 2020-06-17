package net.manikin.main

import net.manikin.serialization.SerializationUtils

object Main {

  import net.manikin.example._

  def main(args: Array[String]): Unit = {
    bank.SimpleTransfer.main(args)
    bank.AdvancedTransfer.main(args)
    polymorph.Main.main(args)
    memoization.Factorial.main(args)
    memoization.Fibonacci.main(args)
  }
}