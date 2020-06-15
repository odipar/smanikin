package net.manikin.example.bank

object SimpleTransfer {
  import net.manikin.core.TransObject._
  import net.manikin.core.context.DefaultContext._
  import IBAN._
  import scala.language.implicitConversions

  def main(args: Array[String]): Unit = {
    implicit val ctx = DefaultContext()

    val a1 = Account.Id(iban = IBAN("A1"))
    val a2 = Account.Id(iban = IBAN("A2"))
    val t1 = Transfer.Id(id = 1)
    val t2 = Transfer.Id(id = 2)

    a1 ! Account.Open(initial = 80.0)
    a2 ! Account.Open(initial = 120.0)
    t1 ! Transfer.Create(_from = a1, _to = a2, _amount = 30.0)
    t2 ! Transfer.Create(_from = a1, _to = a2, _amount = 40.0)
    t2 ! Transfer.Book()
    t1 ! Transfer.Book()

    println("a1: " + ctx(a1).obj) // a1: StateObject(Data(10.0),Opened)
    println("a2: " + ctx(a2).obj) // a2: StateObject(Data(190.0),Opened)
    println("t1: " + ctx(t1).obj) // t1: StateObject(Data(Id(IBAN(A1)),Id(IBAN(A2)),30.0),Booked)
    println("t2: " + ctx(t2).obj) // t1: StateObject(Data(Id(IBAN(A1)),Id(IBAN(A2)),40.0),Booked)
  }
}
