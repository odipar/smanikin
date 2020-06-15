package net.manikin.example.memoization

object Factorial {
  import net.manikin.core.context.store.InMemoryStore._
  import net.manikin.core.TransObject._
  import net.manikin.core.context.Transactor._
  import net.manikin.core.context.DefaultContext._
  import net.manikin.core.state.StateObject._

  case class Factorial(arg: Long) extends StateId[Long] {
    def initData = -1
  }

  case class Calculate(f: Factorial) extends Transaction[Long] {
    def arg = f.arg

    def eff = f.state match {
      case "Initial" => f ! Memorize {
        if (arg < 2) arg
        else arg * (self ! Calculate(Factorial(arg - 1)))
      }
      case "Memorized" => { println("memorized: " + f) ; f.data }
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
    val store = new InMemoryStore() // The Transactors share the same backing Store

    val tx1 = Transactor(DefaultContext(store))
    val tx2 = Transactor(DefaultContext(store))
    
    val r1 = tx1.commit(TId(), Calculate(Factorial(5)))
    println("Factorial(5): " + r1)
    val r2 = tx2.commit(TId(), Calculate(Factorial(10))) // will re-use the memoized version of Factorial via the Store
    println("Factorial(10): " + r2)
  }
}
