package net.manikin.example.memoization

object Fibonacci {
  import net.manikin.core.context.store.slick.postgres.PostgresStore._
  import net.manikin.core.TransObject._
  import net.manikin.core.context.Transactor._
  import net.manikin.core.context.StoreWorld._
  import net.manikin.core.state.StateObject._

  case class Fibonacci(arg: Long) extends StateId[Long] {
    def initData = -1
  }

  case class Calculate(f: Fibonacci) extends Transaction[Long] {
    def arg = f.arg

    def eff = {
      f.abstractState match {
        case "Initial" => f ! Memorize {
          if (arg < 2) arg
          else (self ! Calculate(Fibonacci(arg - 1))) + (self ! Calculate(Fibonacci(arg - 2)))
        }
        case "Memorized" => { println("memorized: " + f) ; f.state }
      }
    }
  }

  case class Memorize(result: Long) extends StateMessage[Fibonacci, Long, Long] {
    def arg = self.arg

    def nst = { case _ => "Memorized" }
    def pre = true
    def apl = result
    def eff = state
    def pst = {
      if (arg < 2) state == arg
      else state == (Fibonacci(arg - 1).state + Fibonacci(arg - 2).state)
    }
  }

  def main(args: Array[String]): Unit = {
    val store = new PostgresStore() // The Transactors share the same backing Store

    val tx1 = Transactor(new StoreWorld(store))
    val tx2 = Transactor(new StoreWorld(store))

    val r1 = tx1.commit(TId(), Calculate(Fibonacci(20))) // re-uses the internal memoized version of Fibonacci
    println("Fibonacci(20): " + r1)
    val r2 = tx2.commit(TId(), Calculate(Fibonacci(30))) // will re-use the memoized version of Fibonacci via the Store
    println("Fibonacci(30): " + r2)
  }
}
