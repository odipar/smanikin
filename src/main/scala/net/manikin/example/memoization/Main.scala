package net.manikin.example.memoization

object Main {
  import net.manikin.core.DefaultContext._
  import net.manikin.core.TransactionalObject._

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
      case Some(x) => x
      case None => self ! Memorize {
        if (arg < 2) arg
        else arg * (Factorial(arg - 1) ! Calculate())
      }
    }
    def pst = (arg > 1) implies self.obj.get == (self.arg * Factorial(self.arg - 1).obj.get)
  }

  case class Memorize(i: Long) extends FactorialMsg {
    def pre = self.obj.isEmpty
    def app = Some(i)
    def eff = self.obj.get
    def pst = self.obj.nonEmpty
  }

  def main(args: Array[String]): Unit = {
    implicit val c = new DefaultContext()

    val r1 = Factorial(5)  ! Calculate()
    val r2 = Factorial(10) ! Calculate()  // will re-used the memoized version of Factorial(5)

    println("r1: " + r1)
    println("r2: " + r2)
  }
}
