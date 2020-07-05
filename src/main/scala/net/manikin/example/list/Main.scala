package net.manikin.example.list

object Main {
  import net.manikin.core.TransObject._
  import java.util.UUID
  import net.manikin.core.context.DefaultContext.DefaultContext

  case class ListId[+X](uuid: UUID = UUID.randomUUID()) extends Id[ListData[X]] {
    def init = Nil

    def asString(implicit c: Context): String = {
      var id = this ; var s = ""
      while (id.obj != Nil) { s = " " + id.obj.head.toString + s ; id = id.obj.tail }
      "(" + s + " )"
    }
  }
  
  trait ListData[+X] {
    def head: X
    def tail: ListId[X]
  }
  
  object Nil extends ListData[Nothing] {
    def head = sys.error("no head")
    def tail = sys.error("no tail")
  }
  
  case class Pair[+X](head: X, tail: ListId[X]) extends ListData[X]

  trait ListMsg[+X, +R] extends Message[ListData[X], ListId[X], R] {
    def list = self.obj
    def old_list = self.old_obj
  }

  case class Create[+X](head: X, tail: ListId[X]) extends ListMsg[X, ListId[X]] {
    def pre = true
    def app = Pair(head, tail)
    def eff = self
    def pst = list.head == head && list.tail == tail
  }

  case class Append[+X](x: X) extends ListMsg[X, ListId[X]] {
    def pre = true
    def app = list
    def eff = ListId[X]() ! Create(x, self)
    def pst = list == old_list
  }

  def main(args: Array[String]): Unit = {
    implicit val c = DefaultContext()

    val l0 = ListId[Int]()
    val l1 = l0 ! Append(1) ! Append(2)
    val l2 = l1 ! Append(3) ! Append(4)

    println("l1: " + l1.asString)
    println("l2: " + l2.asString)
  }
}