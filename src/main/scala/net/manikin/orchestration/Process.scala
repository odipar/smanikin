package net.manikin.orchestration

// Simple Process that will process and retry Tasks in sequence
object Process {
  import net.manikin.core.TransObject.Context
  import net.manikin.core.state.StateObject.{StateId, StateMessage}
  import scala.util.Try

  case class Id[+S](id: String, initial: S) extends StateId[Data[S]] {
    def initData = Data(processData = initial)
  }

  case class Data[+S](processData: S, step: Int = 0, failures: Int = 0, steps: Seq[Task[S]] = Seq())

  trait Msg[S] extends StateMessage[Id[S], Data[S], S]

  abstract class Task[+S](name: String = "") extends Cloneable {
    implicit var ctx: Context = _
    var d: Any = _

    def data: S = d.asInstanceOf[S]

    def apply[S2 >: S](data: S2, ctx: Context): S2 = {
      val t = this.clone().asInstanceOf[this.type] // create throw away copy in order to not pollute the original

      t.d = data // inject Context and State
      t.ctx = ctx

      t.eff
    }
    def eff: S
  }

  case class Add[S](s: Task[S]) extends Msg[S] {
    def nst = { case "Initial" | "Define" => "Define" }
    def pre = true
    def apl = data.copy(steps = data.steps :+ s)
    def eff = data.processData
    def pst = data.steps == old_data.steps :+ s
  }

  case class Do[S]() extends Msg[S] {
    def nst = { case "Define" | "Running" => "Running" }
    def pre = true
    def apl = data
    def eff = {
      def task = data.steps(data.step)

      Try( self ! Success[S](task(data.processData, context)) ) getOrElse {  // Can we process the Task?
        Try( self ! Failure[S]() ) getOrElse {                               // No, send Failure
          Try( self ! Failed[S]() ) getOrElse {                              // Failure fails, the Process has Failed
            data.processData
          }
        }
      }
    }
    def pst = true
  }

  case class Success[S](s: S) extends Msg[S] {
    def nst = { case "Running" => "Running" }
    def pre = true
    def apl = data.copy(processData = s, step = data.step + 1)
    def eff = if (data.step >= data.steps.size) self ! Done[S]() ; else data.processData
    def pst = true
  }

  case class Failure[S]() extends Msg[S] {
    def nst = { case "Running" => "Running" }
    def pre = data.failures < 3 // retry three times
    def apl = data.copy(failures = self.data.failures + 1)
    def eff = data.processData
    def pst = data.failures == old_data.failures + 1
  }

  case class Failed[S]() extends Msg[S] {
    def nst = { case "Running" => "Failed" }
    def pre = data.failures == 3
    def apl = data
    def eff = data.processData
    def pst = true
  }

  case class Done[S]() extends Msg[S] {
    def nst = { case "Running" => "Done" }
    def pre = data.step == self.data.steps.size
    def apl = data
    def eff = data.processData
    def pst = true
  }
}