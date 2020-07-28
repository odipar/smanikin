package net.manikin.example.cryptolist

object Main {
  import net.manikin.core.TransObject._
  import net.manikin.core.context.Transactor._
  import net.manikin.core.context.StoreContext.StoreContext
  import net.manikin.serialization.SerializationUtils._
  import scala.language.implicitConversions

  // Cryptographic List ('block' chain)
  case class ListId[+X](sha256: Seq[Byte] = digest(Nil).toIndexedSeq) extends Id[ListData[X]] {
    def init = Nil
  }

  implicit class ListOps[+X](self: ListId[X])(implicit c: Context) {
    def append[X2 >: X](x: X2) = {
      val pair = Pair(x, self)
      val id = ListId[X2](digest(pair).toIndexedSeq)

      if (id.version > 0) id     // already stored
      else id ! Create(pair)     // create
    }

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

  trait ListMsg[X, +R] extends Message[ListId[X], ListData[X], R]

  case class Create[X](pair: Pair[X]) extends ListMsg[X, ListId[X]] {
    def pre = self.obj == Nil
    def app = pair
    def eff = self
    def pst = obj != Nil && obj.head == pair.head && obj.tail == pair.tail
  }

  def main(args: Array[String]): Unit = {
    implicit val ctx = new StoreContext()

    val l0 = ListId()
    val l1 = l0.append(1).append(2)
    val l2 = l1.append(3).append("a")
    val l3 = l0.append(1).append(2)   // already stored

    println("l1: " + l1)
    println("l2: " + l2)
    println("l3: " + l3)

    println("l1: " + l1.asString)
    println("l2: " + l2.asString)
    println("l3: " + l3.asString)

    val i1: Int = l1.head
    val i2: Any = l2.head // covariance (Int | String => Any)
    
    println("i1: " + i1)
    println("i2: " + i2)
  }
}