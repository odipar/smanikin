package net.manikin.example.memoization

object Fibonacci {
  import net.manikin.core.InMemoryStore._
  import net.manikin.core.TransObject._
  import net.manikin.core.Transactor._
  import net.manikin.core.DefaultContext._
  import net.manikin.core.StateObject._

  case class Fibonacci(arg: Long) extends StateId[Long] {
    def initData = -1
  }

  trait FibonacciMsg extends StateMessage[Long, Fibonacci, Long]

  case class Calculate(f: Fibonacci) extends Transaction[Long] {
    def arg = f.arg

    def eff = f.state match {
      case "Memorized" => { println("memorized: " + f) ; f.data }
      case _ => f ! Memorize {
        if (arg < 2) f.arg
        else (self ! Calculate(Fibonacci(arg - 1))) + (self ! Calculate(Fibonacci(arg - 2)))
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
    val s = new InMemoryStore()

    val tx1 = Transactor(DefaultContext(store = s))
    val tx2 = Transactor(DefaultContext(store = s))

    val r1 = tx1.commit(TId(), Calculate(Fibonacci(5)))
    val r2 = tx2.commit(TId(), Calculate(Fibonacci(10)))  // will re-use the memoized version of Fibonacci(5) via the Store

    println("r1: " + r1)
    println("r2: " + r2)
  }
}
