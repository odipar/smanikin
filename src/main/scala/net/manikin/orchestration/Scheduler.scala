package net.manikin.orchestration

// Simple FIFO scheduler of Processes
object Scheduler {
  import scala.collection.immutable.Queue
  import net.manikin.core.TransObject

  case class Id(id: String) extends TransObject.Id[Data] {
    def init = Data()
  }

  case class Data(queue: Queue[Process.Id[Any]] = Queue())

  trait SchedulerMsg[+R] extends TransObject.Message[Id, Data, R]

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

      if (!(p.state == "Done" || p.state == "Failed")) {
        p ! Process.Do[Any]()
        self ! Add(p) // Reschedule
      }
    }
    def pst = obj.queue.size <= old_obj.queue.size
  }
}
