package net.manikin.example.bank

object Main {
  import net.manikin.core.TransactionalObject._
  import net.manikin.core.TransactionContext._
  import IBAN._

  def main(args: Array[String]): Unit = {
    implicit val c: Context = new TransactionContext()

    val a1 = Account.Id(iban = IBAN("A1"))
    val a2 = Account.Id(iban = IBAN("A2"))
    val t1 = Transaction.Id(id = 1)
    val t2 = Transaction.Id(id = 2)

    a1 <~ Account.Open(initial = 80.0)
    a2 <~ Account.Open(initial = 120.0)
    t1 <~ Transaction.Create(from = a1, to = a2, amount = 30.0)
    t1 <~ Transaction.Commit()
    t2 <~ Transaction.Create(from = a1, to = a2, amount = 20.0)
    t2 <~ Transaction.Commit()

    println("a1: " + c(a1)) // a1: State(Data(30.0),Opened)
    println("a2: " + c(a2)) // a2: State(Data(170.0),Opened)
    println("t1: " + c(t1)) // t1: State(Data(Id(IBAN(A1)),Id(IBAN(A2)),30.0),Committed)
    println("t2: " + c(t2)) // t2: State(Data(Id(IBAN(A1)),Id(IBAN(A2)),20.0),Committed)     
  }
}

