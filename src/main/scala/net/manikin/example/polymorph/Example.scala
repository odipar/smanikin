package net.manikin.example.polymorph

object Example {

  import net.manikin.core.TransactionalObject._
  import net.manikin.core.TransactionContext.TransactionContext

  trait Base {
    def item: String
    def setItem(item: String): Base
  }

  trait BId extends Id[Base] {
    // polymorphic select of Message
    def setItemMessage(item: String): BaseMessage[Unit] = SetItem(item)
    def setItem(item: String)(implicit c: Context): Unit = this <~ setItemMessage(item)
  }

  trait BaseMessage[+R] extends Message[Base, BId, R]

  case class SetItem(item: String) extends BaseMessage[Unit] {
    def pre = !item.contains("$")
    def app = self() = self().setItem(item)
    def pst = true
  }

  case class Extend1(member1: String = "", item: String = "") extends Base {
    def setItem(item: String) = this.copy(item = item)
  }

  case class Extend2(member2: String = "", item: String = "") extends Base {
    def setItem(item: String) = this.copy(item = item)
  }
  
  trait SetItemExtend extends BaseMessage[Unit] {
    def item: String
    def app = self <~ SetItem(item)
    def pst = true
  }

  case class SetItemExtend1(item: String) extends SetItemExtend {
    def pre = !item.contains("~")
  }

  case class SetItemExtend2(item: String) extends SetItemExtend {
    def pre = !item.contains("&")
  }

  case class EId1(id: Long) extends BId {
    def init = Extend1()
  }

  case class EId2(id: Long) extends BId {
    def init = Extend2()
    override def setItemMessage(item: String) = SetItemExtend2(item)
  }

  def main(args: Array[String]): Unit = {
    implicit val c: Context = new TransactionContext()

    val e1: BId = EId1(1)
    val e2: BId = EId2(1)

    e1.setItem("e1")
    e2.setItem("e2")

    println("e1: " + c(e1))
    println("e2: " + c(e2))
  }
}