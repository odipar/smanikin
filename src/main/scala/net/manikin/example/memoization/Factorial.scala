package net.manikin.example.memoization

import java.util.UUID

object Factorial {
  import net.manikin.core.InMemoryStore._
  import net.manikin.core.TransObject._
  import net.manikin.core.Transactor._
  import net.manikin.core.DefaultContext._
  import net.manikin.core.StateObject._

  case class Factorial(arg: Long) extends StateId[Long] {
    def initData = -1
  }

  trait FactorialMsg extends StateMessage[Long, Factorial, Long]
  
  case class Calculate(f: Factorial) extends Transaction[Long] {
    def arg = f.arg

    def eff = f.state match {
      case "Memorized" => { println("memorized: " + f) ; f.data }
      case _ => f ! Memorize {
        if (arg < 2) f.arg
        else arg * (self ! Calculate(Factorial(arg - 1)))
      }
    }
  }

  case class Memorize(result: Long) extends StateMessage[Long, Factorial, Long] {
    def arg = self.arg

    def nst = { case _ => "Memorized" }
    def pre = true
    def apl = result
    def eff = data
    def pst = {
      if (arg < 2) data == arg
      else data == arg * Factorial(arg - 1).data
    }
  }

  def main(args: Array[String]): Unit = {
    val s = new InMemoryStore()

    val tx1 = Transactor(DefaultContext(store = s))
    val tx2 = Transactor(DefaultContext(store = s))
    
    val r1 = tx1.commit(TId(), Calculate(Factorial(5)))
    val r2 = tx2.commit(TId(), Calculate(Factorial(10)))  // will re-use the memoized version of Factorial(5) via the Store

    println("r1: " + r1)
    println("r2: " + r2)
  }
}
