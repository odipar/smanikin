package net.manikin.main

object Main {
  import net.manikin.core.InMemoryStore._
  import net.manikin.core.TransObject._
  import net.manikin.core.Transactor._
  import net.manikin.core.DefaultContext._
  import net.manikin.core.StateObject._

  case class MId(i: Long) extends Id[Long] {
    def init = 0
  }

  trait Msg extends Message[Long, MId, Long]

  case class Step1() extends Msg {
    def pre = self.obj == 0
    def app = self.obj + 1
    def eff = self ! Step2()
    def pst = self.obj == self.old_obj + 2
  }

  case class Step2() extends Msg {
    def pre = self.obj > 0
    def app = self.obj + 1
    def eff = self.obj
    def pst = self.obj == self.old_obj + 1
  }

  def main(args: Array[String]): Unit = {
    implicit val c = DefaultContext()

    val id = MId(0)

    val r = id ! Step1()
    println("r: " + r)
    println("c: " + c.sends)

  }
}