package net.manikin.orchestration

import net.manikin.core.TransObject.{Context, Message}
import net.manikin.core.context.Transactor.Transaction
import net.manikin.core.state.StateObject.{StateId, StateMessage, StateObject}

object Main {
  import net.manikin.example.bank.Account
  import net.manikin.example.bank.Transfer
  import net.manikin.example.bank.IBAN.IBAN
  import net.manikin.core.context.ObjectContext.ObjectContext
  import scala.util.Try

  // You can easily try alternative sends that fail: state will always be rolled back to the previous valid state!
  // This is very hard to do with POJOs

  def main(args: Array[String]): Unit = {
    implicit val c = new ObjectContext()

    val a1 = Account.Id(IBAN("A1"))
    val a2 = Account.Id(IBAN("A2"))

    val t1 = Transfer.Id(1)

    a1 ! Account.Open(5)
    a2 ! Account.Open(10)
    
    Try ( t1 ! Transfer.Book(a1, a2, 8) ) getOrElse(   // Fails
      Try ( t1 ! Transfer.Book(a1, a2, 7) ) getOrElse( // Fails
        t1 ! Transfer.Book(a1, a2, 4)                  // Succeeds
      )
    )

    println(c(a1)) // VObject(2,StateObject(Data(1),Opened))
    println(c(a2)) // VObject(2,StateObject(Data(14),Opened))
    println(c(t1)) // VObject(1,StateObject(Data(Id(IBAN(A1)),Id(IBAN(A2)),4),Booked))
  }
}
