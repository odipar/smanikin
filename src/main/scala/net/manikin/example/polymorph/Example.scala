package net.manikin.example.polymorph

object Example {

  import net.manikin.core.TransactionalObject._
  import net.manikin.core.TransactionContext.TransactionContext

  trait Base[B <: Base[B]] {
    def item: String
    def setItem(item: String): B
  }

  trait BId[B <: Base[B]] extends Id[B] {
    // polymorphic selection of Message
    def setItem(item: String): BaseMessage[B, Unit] = SetItem(item)
  }

  trait BaseMessage[B <: Base[B], +R] extends Message[B, BId[B], R] 

  case class SetItem[B <: Base[B]](item: String) extends BaseMessage[B, Unit] {
    def pre = !item.contains("$")
    def app = self() = self().setItem(item)
    def pst = true
  }

  case class Extend1(member1: String = "", item: String = "") extends Base[Extend1] {
    def setItem(item: String) = this.copy(item = item)
  }

  case class Extend2(member2: String = "", item: String = "") extends Base[Extend2] {
    def setItem(item: String) = this.copy(item = item)
  }

  trait SetItemExtend[B <: Base[B]] extends BaseMessage[B, Unit] {
    def item: String
    def app = self <~ SetItem(item)
    def pst = true
  }

  case class SetItemExtend1(item: String) extends SetItemExtend[Extend1] {
    def pre = !item.contains("~")
  }

  case class SetItemExtend2(item: String) extends SetItemExtend[Extend2] {
    def pre = !item.contains("&")
  }

  case class EId1(id: Long) extends BId[Extend1] {
    def init = Extend1()
  }

  case class EId2(id: Long) extends BId[Extend2] {
    def init = Extend2()
    override def setItem(item: String) = SetItemExtend2(item)
  }

  def main(args: Array[String]): Unit = {
    implicit val c: Context = new TransactionContext()

    val e1 = EId1(1)
    val e2 = EId2(1)

    e1 <~ e1.setItem("e1")
    e2 <~ e2.setItem("e2")

    println("e1: " + c(e1))
    println("e2: " + c(e2))
  }
}