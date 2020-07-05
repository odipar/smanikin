package net.manikin.example.memoization


object Fibonacci {
  import net.manikin.core.context.store.slick.postgres.PostgresStore._
  import net.manikin.core.TransObject._
  import net.manikin.core.context.Transactor._
  import net.manikin.core.context.DefaultContext._
  import net.manikin.core.state.StateObject._

  case class Fibonacci(arg: Long) extends StateId[Long] {
    def initData = -1
  }

  case class Calculate(f: Fibonacci) extends Transaction[Long] {
    def arg = f.arg

    def eff = {
      f.state match {
        case "Initial" => f ! Memorize {
          if (arg < 2) arg
          else (self ! Calculate(Fibonacci(arg - 1))) + (self ! Calculate(Fibonacci(arg - 2)))
        }
        case "Memorized" => { println("memorized: " + f) ; f.data }
      }
    }
  }

  case class Memorize(result: Long) extends StateMessage[Long, Fibonacci, Long] {
    def arg = self.arg

    def nst = { case _ => "Memorized" }
    def pre = true
    def apl = result
    def eff = data
    def pst = {
      if (arg < 2) data == arg
      else data == (Fibonacci(arg - 1).data + Fibonacci(arg - 2).data)
    }
  }

  def main(args: Array[String]): Unit = {
    val store = new PostgresStore() // The Transactors share the same backing Store

    val tx1 = Transactor(DefaultContext(store))
    val tx2 = Transactor(DefaultContext(store))

    val r1 = tx1.commit(TId(), Calculate(Fibonacci(20))) // re-uses the internal memoized version of Fibonacci
    println("Fibonacci(20): " + r1)
    val r2 = tx2.commit(TId(), Calculate(Fibonacci(30))) // will re-use the memoized version of Fibonacci via the Store
    println("Fibonacci(30): " + r2)
  }
}
