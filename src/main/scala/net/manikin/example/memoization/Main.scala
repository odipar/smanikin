package net.manikin.example.memoization

object Main {
  import net.manikin.core.InMemoryStore._
  import net.manikin.core.TransObject._
  import net.manikin.core.Transactor._
  import net.manikin.core.DefaultContext._
  import net.manikin.core.StateObject._

  case class Factorial(arg: Long) extends StateId[Long] {
    def initData = -1
  }

  trait FactorialMsg extends StateMessage[Long, Factorial, Long] {
    def arg = self.arg
  }
  
  case class Calculate() extends FactorialMsg {
    def nst = { case x => x }
    def pre = self.arg >= 0
    def apl = data
    def eff = state match {
      case "Memorized" => { println("memorized: " + self) ; data }
      case _ => self ! Memorize {
        if (arg > 1) arg * (Factorial(arg - 1) ! Calculate())
        else arg
      }
    }
    def pst = (arg > 1) implies data == (self.arg * Factorial(self.arg - 1).data)
  }

  case class Memorize(result: Long) extends FactorialMsg {
    def nst = { case _ => "Memorized" }
    def pre = true
    def apl = result
    def eff = data
    def pst = true
  }

  def main(args: Array[String]): Unit = {
    val s = new InMemoryStore()

    val tx1 = Transactor(DefaultContext(store = s))
    val tx2 = Transactor(DefaultContext(store = s))
    
    val r1 = tx1.commit(Factorial(5), Calculate())
    val r2 = tx2.commit(Factorial(10), Calculate())  // will re-use the memoized version of Factorial(5) via the Store

    println("r1: " + r1)
    println("r2: " + r2)
  }
}
