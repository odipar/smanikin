package net.manikin.example.bank

object SimpleTransfer {
  import net.manikin.core.context.EventWorld._
  import IBAN._
  import scala.language.implicitConversions

  def main(args: Array[String]): Unit = {
    implicit val ctx = new EventWorld()

    val a1 = Account.Id(iban = IBAN("A1"))
    val a2 = Account.Id(iban = IBAN("A2"))
    val t1 = Transfer.Id(id = 1)
    val t2 = Transfer.Id(id = 2)

    a1 ! Account.Open(initial = 80)
    a2 ! Account.Open(initial = 120)
    t1 ! Transfer.Book(from = a1, to = a2, amount = 30)
    t2 ! Transfer.Book(from = a1, to = a2, amount = 40)

    println("a1: " + ctx(a1)) // a1: VObject(3,0,StateObject(State(10),Opened))
    println("a2: " + ctx(a2)) // a2: VObject(3,0,StateObject(State(190),Opened))
    println("t1: " + ctx(t1)) // t1: VObject(1,0,StateObject(State(Id(IBAN(A1)),Id(IBAN(A2)),30),Booked))
    println("t2: " + ctx(t2)) // t2: VObject(1,0,StateObject(State(Id(IBAN(A1)),Id(IBAN(A2)),40),Booked))
  }
}
