package net.manikin.example.memoization

import net.manikin.core.InMemoryStore.InMemoryStore
import net.manikin.core.Transactor.Transactor

object Main {
  import net.manikin.core.DefaultContext._
  import net.manikin.core.TransObject._

  case class Factorial(arg: Long) extends Id[Option[Long]] {
    def init = None
  }

  trait FactorialMsg extends Message[Option[Long], Factorial, Long] {
    def arg = self.arg
  }
  
  case class Calculate() extends FactorialMsg {
    def pre = self.arg >= 0
    def app = self.obj
    def eff = self.obj match {
      case Some(x) => { println(s"memoized + $self") ; x }
      case None => self ! Memorize {
        if (arg > 1) arg * (Factorial(arg - 1) ! Calculate())
        else arg
      }
    }
    def pst = (arg > 1) implies self.obj.get == (self.arg * Factorial(self.arg - 1).obj.get)
  }

  case class Memorize(result: Long) extends FactorialMsg {
    def pre = self.obj.isEmpty
    def app = Some(result)
    def eff = self.obj.get
    def pst = self.obj.nonEmpty
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
