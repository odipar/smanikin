package net.manikin.example.list

import net.manikin.core.context.Transactor.{TId, Transaction}

object Main {
  import net.manikin.core.TransObject._
  import java.util.UUID
  import net.manikin.core.context.DefaultContext.DefaultContext
  import scala.language.implicitConversions

  case class ListId[+X](uuid: UUID = UUID.randomUUID()) extends Id[ListData[X]] {
    def init = Nil
  }

  implicit class ListOps[+X](self: ListId[X])(implicit c: Context) {
    def append[X2 >: X](x: X2) = ListId[X2]() ! Create(Pair(x, self))
    def head = self.obj.head
    def tail = self.obj.tail.obj
    def asString = {
      var list = self ; var s = ""
      while (list.tail != Nil) { s = ", " + list.head.toString + s ; list = list.obj.tail }
      "(" + list.head.toString + s + ")"
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

  trait ListMsg[+X, +R] extends Message[ListData[X], ListId[X], R]

  case class Create[+X](pair: Pair[X]) extends ListMsg[X, ListId[X]] {
    def pre = self.obj == Nil
    def app = pair
    def eff = self
    def pst = self.obj != Nil && self.obj.head == pair.head && self.obj.tail == pair.tail
  }

  def main(args: Array[String]): Unit = {

    object T1 extends Transaction[ListId[Any]] {
      def eff = {
        val l0: ListId[Nothing] = ListId()
        val l1: ListId[Int] = l0.append(1).append(2)
        val l2: ListId[Any] = l1.append(3).append("a")

        println("l1: " + l1.asString)
        println("l2: " + l2.asString)
        println("l2.head: " + l2.head)

        l2
      }
    }

    val ctx = DefaultContext()
    val r = (TId() ! T1)(ctx)

    println("r: " + r)
  }
}