package net.manikin.orchestration

import net.manikin.core.context.StoreWorld.StoreWorld
import net.manikin.core.context.store.slick.postgres.PostgresStore.PostgresStore

object Main {
  import net.manikin.core.context.ObjectWorld.ObjectWorld

  trait IntTask extends Process.Task[Int]

  def main(args: Array[String]): Unit = {
    val store = new PostgresStore(tx_uuid = 1)
    implicit val c = new StoreWorld(store)

    val s = Scheduler.Id("s1") // The Scheduler will schedule two Processes

    val p1 = Process.Id("p1", 0) // This Process will Succeed
    val p2 = Process.Id("p2", 1) // This Process will Fail

    val t1 = new IntTask { def eff = data + 1 }
    val t2 = new IntTask { def eff = data * 2 }
    val t3 = new IntTask { def eff = if (data == 2) data - 5; else sys.error("no") }
    val t4 = new IntTask { def eff = data * 3 }

    p1 ! Process.Add(t1)
    p1 ! Process.Add(t2)
    p1 ! Process.Add(t3)
    p1 ! Process.Add(t4)

    p2 ! Process.Add(t1)
    p2 ! Process.Add(t2)
    p2 ! Process.Add(t3)
    p2 ! Process.Add(t4)

    s ! Scheduler.Add(p1)
    s ! Scheduler.Add(p2)

    while(s.obj.queue.nonEmpty) { s ! Scheduler.Do() } // just keep the Scheduler going

    c.commit()
    
    println("p1.state: " + p1.abstractState)
    println("p1.failures: " + p1.state.failures)
    println("p1.data.state: " + p1.state.processData)

    println("p2.state: " + p2.abstractState)
    println("p2.failures: " + p2.state.failures)
    println("p2.data.state: " + p2.state.processData)
  }
}
