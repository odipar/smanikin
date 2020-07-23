package net.manikin.orchestration

object Task {
  import net.manikin.core.context.ObjectContext.ObjectContext
  import net.manikin.core.TransObject.Context
  import net.manikin.core.state.StateObject.{StateId, StateMessage}
  import scala.util.Try

  case class TaskId[S](id: String, initial: S) extends StateId[TaskData[S]] {
    def initData = TaskData(taskData = initial)
  }

  case class TaskData[S](taskData: S, step: Int = 0, failures: Int = 0, steps: Seq[Step[S]] = Seq())

  trait TaskMsg[S] extends StateMessage[TaskId[S], TaskData[S], S]

  abstract class Step[S](name: String = "") extends Cloneable {
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

  case class TaskAdd[S](s: Step[S]) extends TaskMsg[S] {
    def nst = { case "Initial" | "Define" => "Define" }
    def pre = true
    def apl = data.copy(steps = data.steps :+ s)
    def eff = data.taskData
    def pst = data.steps == old_data.steps :+ s
  }

  case class TaskDo[S]() extends TaskMsg[S] {
    def nst = { case "Define" | "Running" => "Running" }
    def pre = true
    def apl = data
    def eff = {
      def step = data.steps(data.step)

      Try(self ! TaskSuccess(step(data.taskData, context))) getOrElse {
        Try(self ! TaskFailure()) getOrElse {
          Try(self ! TaskFailed()) getOrElse {
            data.taskData
          }
        }
      }
    }
    def pst = true
  }

  case class TaskSuccess[S](s: S) extends TaskMsg[S] {
    def nst = { case "Running" => "Running" }
    def pre = true
    def apl = data.copy(taskData = s, step = data.step + 1)
    def eff = {
      if (data.step >= data.steps.size) self ! TaskDone()
      else data.taskData
    }
    def pst = true
  }

  case class TaskFailure[S]() extends TaskMsg[S] {
    def nst = { case "Running" => "Running" }
    def pre = data.failures < 3
    def apl = data.copy(failures = self.data.failures + 1)
    def eff = data.taskData
    def pst = data.failures == old_data.failures + 1
  }

  case class TaskFailed[S]() extends TaskMsg[S] {
    def nst = { case "Running" => "Failed" }
    def pre = true
    def apl = data
    def eff = data.taskData
    def pst = true
  }

  case class TaskDone[S]() extends TaskMsg[S] {
    def nst = { case "Running" => "Done" }
    def pre = data.step == self.data.steps.size
    def apl = data
    def eff = data.taskData
    def pst = true
  }

  def main(args: Array[String]): Unit = {
    implicit val c = new ObjectContext()

    val t1 = TaskId("TestTask", 0)

    val s1 = new Step[Int]("first") { def eff = data + 1 }
    val s2 = new Step[Int] { def eff = data * 2 }
    val s3 = new Step[Int] { def eff = if (data == 2) data - 5; else sys.error("no") }
    val s4 = new Step[Int]("last") { def eff = data * 3 }


    t1 ! TaskAdd(s1)
    t1 ! TaskAdd(s2)
    t1 ! TaskAdd(s3)
    t1 ! TaskAdd(s4)

    while(t1.state != "Done" && t1.state != "Failed") {
      val s = t1 ! TaskDo()
      println("state: " + t1.state)
      println("failures: " + t1.data.failures)
      println("taskState: " + t1.data.taskData)
    }

  }
}