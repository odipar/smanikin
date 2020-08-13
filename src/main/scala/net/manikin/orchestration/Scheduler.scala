package net.manikin.orchestration

// Simple FIFO scheduler of Processes
object Scheduler {
  import scala.collection.immutable.Queue
  import net.manikin.core.TransObject

  case class Id(id: String) extends TransObject.Id[State] {
    def init = State()
  }

  case class State(queue: Queue[Process.Id[Any]] = Queue())

  trait SchedulerMsg[+R] extends TransObject.Message[Id, State, R]

  case class Add(p: Process.Id[Any]) extends SchedulerMsg[Unit] {
    def pre = true
    def app = obj.copy(queue = obj.queue.enqueue(p))
    def eff = { }
    def pst = obj.queue == old_obj.queue.enqueue(p)
  }

  case class Do() extends SchedulerMsg[Unit] {
    def pre = obj.queue.nonEmpty
    def app = obj.copy(queue = obj.queue.dequeue._2)
    def eff = {
      val p = old_obj.queue.dequeue._1

      if (!(p.abstractState == "Done" || p.abstractState == "Failed")) {
        p ! Process.Do[Any]()
        self ! Add(p) // Reschedule
      }
    }
    def pst = obj.queue.size <= old_obj.queue.size
  }
}
