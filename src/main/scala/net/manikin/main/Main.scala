package net.manikin.main

object Main {

  import net.manikin.example._

  def main(args: Array[String]): Unit = {
    bank.Main.main(args)
    polymorph.Main.main(args)
    memoization.Factorial.main(args)
    memoization.Fibonacci.main(args)
  }
}