package net.manikin.example.polymorph

object Main {
  import net.manikin.core.TransObject._
  import net.manikin.core.state.StateObject._
  import net.manikin.core.context.DefaultContext.DefaultContext

  trait Base {
    def item: String
    def setItem(item: String): Base
  }

  trait BId extends StateId[Base] {
    // polymorphic select of Message
    def setItem(item: String)(implicit c: Context): Unit = this ! SetItem(item)
  }

  trait BaseMessage[+R] extends StateMessage[Base, BId, R] {
    def nst = { case x => x }
  }

  case class SetItem(item: String) extends BaseMessage[Unit] {
    def pre = !item.contains("$")
    def apl = data.setItem(item)
    def eff = { }
    def pst = true
  }

  case class Extend1(member1: String = "", item: String = "") extends Base {
    def setItem(item: String): Base = this.copy(item = item)
  }
  case class Extend2(var member2: String = "", item: String = "") extends Base {
    def setItem(item: String): Base = this.copy(item = item)
  }

  trait SetItemExtend extends BaseMessage[Unit] {
    def item: String

    def apl = data
    def eff = self ! SetItem(item)
    def pst = true
  }

  case class SetItemExtend1(item: String) extends SetItemExtend {
    def pre = !item.contains("~")
  }

  case class SetItemExtend2(item: String) extends SetItemExtend {
    def pre = !item.contains("&")
  }

  case class EId1(id: Long) extends BId {
    def initData = Extend1()
  }

  case class EId2(id: Long) extends BId {
    def initData = Extend2()
    override def setItem(item: String)(implicit c: Context): Unit = this ! SetItemExtend2(item)
  }

  def main(args: Array[String]): Unit = {
    implicit val c = DefaultContext()

    val e1: BId = EId1(1)
    val e2: BId = EId2(1)

    e1.setItem("e1")
    e2.setItem("e2")

    println("e1: " + c(e1))
    println("e2: " + c(e2))
  }
}