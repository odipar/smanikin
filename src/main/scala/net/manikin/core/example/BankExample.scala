package net.manikin.core.example

object BankExample {
  import net.manikin.core.asm.TransactionalObject._
  import net.manikin.core.example.IBAN.IBAN

  def main(args: Array[String]): Unit = {
    implicit val c = new TransactionContext()

    val a1 = Account.Id(iban = IBAN("A1"))
    val a2 = Account.Id(iban = IBAN("A2"))
    val t1 = Transaction.Id(id = 1)
    val t2 = Transaction.Id(id = 2)

    a1 <-- Account.Open(initial = 80.0)
    a2 <-- Account.Open(initial = 120.0)
    t1 <-- Transaction.Create(from = a1, to = a2, amount = 30.0)
    t2 <-- Transaction.Commit()
    t2 <-- Transaction.Create(from = a1, to = a2, amount = 20.0)
    t2 <-- Transaction.Commit()

    println("c: " + c.versions)

  }
}

