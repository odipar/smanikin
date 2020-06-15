package net.manikin.example.bank

object SimpleTransfer {
  import net.manikin.core.TransObject._
  import net.manikin.core.context.DefaultContext._

  import Account._
  import Transfer._
  import IBAN._

  def main(args: Array[String]): Unit = {
    implicit val ctx = DefaultContext()

    val a1 = AccountId(iban = IBAN("A1"))
    val a2 = AccountId(iban = IBAN("A2"))
    val t1 = TransferId(id = 1)
    val t2 = TransferId(id = 2)

    a1 ! Open(initial = 80.0)
    a2 ! Open(initial = 120.0)
    t1 ! Create(from = a1, to = a2, amount = 30.0)
    t2 ! Create(from = a1, to = a2, amount = 40.0)
    t2 ! Book()
    t1 ! Book()

    println("a1: " + ctx(a1).obj) // a1: StateObject(Data(10.0),Opened)
    println("a2: " + ctx(a2).obj) // a2: StateObject(Data(190.0),Opened)
    println("t1: " + ctx(t1).obj) // t1: StateObject(Data(Id(IBAN(A1)),Id(IBAN(A2)),30.0),Booked)
    println("t2: " + ctx(t2).obj) // t1: StateObject(Data(Id(IBAN(A1)),Id(IBAN(A2)),40.0),Booked)
  }
}
