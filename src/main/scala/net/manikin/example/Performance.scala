package net.manikin.example

object Performance {
  import net.manikin.core.Core._
  import net.manikin.message.ThreadLocal._
  import net.manikin.world.SimpleWorld._

  case class CounterId(id: Long) extends Id[Long] {
    def init = 0
  }

  case class Increase[W <: World[W]]() extends LMsg[W, CounterId, Long, Unit] {
    def pre = true
    def app = obj + 1
    def eff = ()
    def pst = obj == old + 1
  }

  def main(args: Array[String]): Unit = {
    val id = CounterId(0)
    val msg = Increase[SimpleWorld]()

    var world = SimpleWorld()

    val x = 10000000
    time("took") {
      for (i <- 0 until x) {
        world = world.send(id, msg).world
        if ((i % 1000000) == 0) println("messages: " + i)
      }
      println(world.obj(id).value)
    }
  }

  def time[R](key: String)(block: => R): R = {
    val t0 = System.currentTimeMillis().toDouble
    val result = block
    val t1 = System.currentTimeMillis().toDouble
    println(key + " - elapsed time: " + (t1 - t0) + "ms")
    result
  }
}
