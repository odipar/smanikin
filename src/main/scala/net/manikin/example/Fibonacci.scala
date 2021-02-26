package net.manikin.example

object Fibonacci {
  import net.manikin.core.Core._
  import net.manikin.message.StateObject._
  import net.manikin.message.Transaction._
  import net.manikin.world.SimpleWorld._

  case class Fibonacci(arg: Long) extends SId[Long] {
    def ini = -1
  }

  case class Calculate[W <: World[W]](f: Fibonacci) extends Do[W, Long] {
    def arg = f.arg

    def eff = {
      state(f) match {
        case "Initial" => send(f, Memorize {
          if (arg < 2) arg
          else send(self, Calculate(Fibonacci(arg - 1))) + send(self, Calculate(Fibonacci(arg - 2)))
        })
        case "Memorized" => { println("memorized: " + f); obj(f) }
      }
    }

    case class Memorize[W <: World[W]](result: Long) extends SMsg[W, Fibonacci, Long, Long] {
      def arg = self.arg

      def nst = { case "Initial" => "Memorized" }
      def pre = true
      def app = result
      def eff = obj
      def pst = {
        if (arg < 2) obj == arg
        else obj == obj(Fibonacci(arg - 1)) + obj(Fibonacci(arg - 2))
      }
    }
  }


  def main(arg: Array[String]): Unit = {
    val world = SimpleWorld()

    val fib20 = world.send(TID(), Calculate(Fibonacci(20)))
    val fib30 = fib20.world.send(TID(), Calculate(Fibonacci(30)))

    println("fib20: " + fib20.value)
    println("fib30: " + fib30.value)
  }
}
