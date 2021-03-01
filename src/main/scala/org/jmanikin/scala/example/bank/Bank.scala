package org.jmanikin.scala.example.bank

// The Bank example with less boilerplate
object Bank {
  import org.jmanikin.core._
  import org.jmanikin.scala.message.ScalaMessage._
  import org.jmanikin.world._

  case class AccountId(iban: String) extends Id[Account] {def init = Account()}
  case class Account(balance: Double = 0.0)
  trait AccountMsg[W <: World[W]] extends ScalaMessage[W, AccountId, Account, Unit]

  case class Open[W <: World[W]](initial: Double) extends AccountMsg[W] {
    def scala =
      pre {initial > 0.0}.
        app {obj.copy(balance = initial)}.
        eff {}.
        pst {obj.balance == initial}
  }

  case class Deposit[W <: World[W]](amount: Double) extends AccountMsg[W] {
    def scala =
      pre {amount > 0.0}.
        app {obj.copy(balance = obj.balance + amount)}.
        eff {}.
        pst {obj.balance == old.balance + amount}
  }

  case class Withdraw[W <: World[W]](amount: Double) extends AccountMsg[W] {
    def scala =
      pre {amount > 0.0}.
        app {obj.copy(balance = obj.balance - amount)}.
        eff {}.
        pst {obj.balance == old.balance - amount}
  }

  case class TransferId(id: Long) extends Id[Transfer] {def init = Transfer()}
  case class Transfer(from: AccountId = null, to: AccountId = null, amount: Double = 0.0)

  trait TransferMsg[W <: World[W]] extends ScalaMessage[W, TransferId, Transfer, Unit]

  case class Book[W <: World[W]](from: AccountId, to: AccountId, amount: Double) extends TransferMsg[W] {
    def scala =
      pre {amount > 0.0 && from != to}.
        app {Transfer(from, to, amount)}.
        eff {send(from, Withdraw(amount)); send(to, Deposit(amount))}.
        pst {obj(from).balance + obj(to).balance == old(from).balance + old(to).balance}
  }

  def main(args: Array[String]): Unit = {
    val a1 = AccountId("A1")
    val a2 = AccountId("A2")
    val t1 = TransferId(1)

    val r = new SimpleWorld().
      send(a1, Open(50)).
      send(a2, Open(80)).
      send(t1, Book(a1, a2, 30))

    println("a1: " + r.obj(a1).value) // a1: Account(20.0)
    println("a2: " + r.obj(a2).value) // a2: Account(110.0)
  }
}
