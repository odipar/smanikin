package net.manikin.orchestration

object Process {
  import net.manikin.core.context.ObjectContext.ObjectContext
  import net.manikin.core.TransObject.Context
  import net.manikin.core.state.StateObject.{StateId, StateMessage}
  import scala.util.Try

  case class ProcessId[S](id: String, initial: S) extends StateId[ProcessData[S]] {
    def initData = ProcessData(processData = initial)
  }

  case class ProcessData[S](processData: S, step: Int = 0, failures: Int = 0, steps: Seq[Task[S]] = Seq())

  trait ProcessMsg[S] extends StateMessage[ProcessId[S], ProcessData[S], S]

  abstract class Task[S](name: String = "") extends Cloneable {
    implicit var ctx: Context = _
    var data: S = _

    def apply(data: S, ctx: Context): S = {
      val t = this.clone().asInstanceOf[this.type] // create throw away copy in order to not pollute the original
      // inject Context and State
      t.data = data
      t.ctx = ctx
      t.eff
    }
    def eff: S
  }

  case class ProcessAdd[S](s: Task[S]) extends ProcessMsg[S] {
    def nst = { case "Initial" | "Define" => "Define" }
    def pre = true
    def apl = data.copy(steps = data.steps :+ s)
    def eff = data.processData
    def pst = data.steps == old_data.steps :+ s
  }

  case class ProcessDo[S]() extends ProcessMsg[S] {
    def nst = { case "Define" | "Running" => "Running" }
    def pre = true
    def apl = data
    def eff = {
      def task = data.steps(data.step)

      Try(self ! ProcessSuccess(task(data.processData, context))) getOrElse {  // Can we apply the Task?
        Try(self ! ProcessFailure()) getOrElse {  // No, signal Failure
          Try(self ! ProcessFailed()) getOrElse { // To many Failures, the Process has Failed
            data.processData
          }
        }
      }
    }
    def pst = true
  }

  case class ProcessSuccess[S](s: S) extends ProcessMsg[S] {
    def nst = { case "Running" => "Running" }
    def pre = true
    def apl = data.copy(processData = s, step = data.step + 1)
    def eff = if (data.step >= data.steps.size) self ! ProcessDone() ; else data.processData
    def pst = true
  }

  case class ProcessFailure[S]() extends ProcessMsg[S] {
    def nst = { case "Running" => "Running" }
    def pre = data.failures < 3
    def apl = data.copy(failures = self.data.failures + 1)
    def eff = data.processData
    def pst = data.failures == old_data.failures + 1
  }

  case class ProcessFailed[S]() extends ProcessMsg[S] {
    def nst = { case "Running" => "Failed" }
    def pre = true
    def apl = data
    def eff = data.processData
    def pst = true
  }

  case class ProcessDone[S]() extends ProcessMsg[S] {
    def nst = { case "Running" => "Done" }
    def pre = data.step == self.data.steps.size
    def apl = data
    def eff = data.processData
    def pst = true
  }

  def main(args: Array[String]): Unit = {
    implicit val c = new ObjectContext()

    val p1 = ProcessId("p1", 0)

    trait IntTask extends Task[Int]

    val t1 = new IntTask { def eff = data + 1 }
    val t2 = new IntTask { def eff = data * 2 }
    val t3 = new IntTask { def eff = if (data == 2) data - 5; else sys.error("no") }
    val t4 = new IntTask { def eff = data * 3 }


    p1 ! ProcessAdd(t1)
    p1 ! ProcessAdd(t2)
    p1 ! ProcessAdd(t3)
    p1 ! ProcessAdd(t4)

    while(p1.state != "Done" && p1.state != "Failed") {
      p1 ! ProcessDo()
      
      println("state: " + p1.state)
      println("failures: " + p1.data.failures)
      println("taskState: " + p1.data.processData)
    }

  }
}