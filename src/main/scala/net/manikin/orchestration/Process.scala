package net.manikin.orchestration

// Simple Process that will process and retry Tasks in sequence
object Process {
  import net.manikin.core.TransObject.World
  import net.manikin.core.state.StateObject.{StateId, StateMessage}
  import scala.util.Try

  case class Id[+S](id: String, initial: S) extends StateId[State[S]] {
    def initData = State(processData = initial)
  }

  case class State[+S](processData: S, step: Int = 0, failures: Int = 0, steps: Seq[Task[S]] = Seq())

  trait Msg[S] extends StateMessage[Id[S], State[S], S]

  abstract class Task[+S](name: String = "") extends Cloneable {
    implicit var ctx: World = _
    var d: Any = _

    def data: S = d.asInstanceOf[S]

    def apply[S2 >: S](data: S2, ctx: World): S2 = {
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
    def apl = state.copy(steps = state.steps :+ s)
    def eff = state.processData
    def pst = state.steps == old_state.steps :+ s
  }

  case class Do[S]() extends Msg[S] {
    def nst = { case "Define" | "Running" => "Running" }
    def pre = true
    def apl = state
    def eff = {
      def task = state.steps(state.step)

      Try( self ! Success[S](task(state.processData, context)) ) getOrElse {  // Can we process the Task?
        Try( self ! Failure[S]() ) getOrElse {                               // No, send Failure
          Try( self ! Failed[S]() ) getOrElse {                              // Failure fails, the Process has Failed
            state.processData
          }
        }
      }
    }
    def pst = true
  }

  case class Success[S](s: S) extends Msg[S] {
    def nst = { case "Running" => "Running" }
    def pre = true
    def apl = state.copy(processData = s, step = state.step + 1)
    def eff = if (state.step >= state.steps.size) self ! Done[S]() ; else state.processData
    def pst = true
  }

  case class Failure[S]() extends Msg[S] {
    def nst = { case "Running" => "Running" }
    def pre = state.failures < 3 // retry three times
    def apl = state.copy(failures = self.state.failures + 1)
    def eff = state.processData
    def pst = state.failures == old_state.failures + 1
  }

  case class Failed[S]() extends Msg[S] {
    def nst = { case "Running" => "Failed" }
    def pre = state.failures == 3
    def apl = state
    def eff = state.processData
    def pst = true
  }

  case class Done[S]() extends Msg[S] {
    def nst = { case "Running" => "Done" }
    def pre = state.step == self.state.steps.size
    def apl = state
    def eff = state.processData
    def pst = true
  }
}