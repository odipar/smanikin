package org.jmanikin.scala.example.bank

// The Bank example with less boilerplate
object Bank {

  import org.jmanikin.core._
  import org.jmanikin.scala.message.ScalaMessage._
  import org.jmanikin.scala.world.EventWorld._

  case class AccountId(iban: String) extends Id[Account] { def init = Account() }
  case class Account(balance: Double = 0.0)
  trait AccountMsg extends ScalaMessage[AccountId, Account, Unit]

  case class Open(initial: Double) extends AccountMsg {
    def scala =
      pre { initial > 0.0 }.
      app { obj.copy(balance = initial) }.
      eff { }.
      pst { obj.balance == initial }
  }

  case class Deposit(amount: Double) extends AccountMsg {
    def scala =
      pre { amount > 0.0 }.
      app { obj.copy(balance = obj.balance + amount) }.
      eff { }.
      pst { obj.balance == old.balance + amount }
  }

  case class Withdraw(amount: Double) extends AccountMsg {
    def scala =
      pre { amount > 0.0 && obj.balance >= amount }.
      app { obj.copy(balance = obj.balance - amount) }.
      eff { }.
      pst { obj.balance == old.balance - amount }
  }

  case class TransferId(id: Long) extends Id[Transfer] { def init = Transfer() }
  case class Transfer(from: AccountId = null, to: AccountId = null, amount: Double = 0.0)
  trait TransferMsg extends ScalaMessage[TransferId, Transfer, Unit]

  case class Book(from: AccountId, to: AccountId, amount: Double) extends TransferMsg {
    def scala =
      pre { amount > 0.0 && from != to }.
      app { Transfer(from, to, amount) }.
      eff { send(from, Withdraw(amount)); send(to, Deposit(amount)) }.
      pst { obj(from).balance + obj(to).balance == old(from).balance + old(to).balance }
  }

  def main(args: Array[String]): Unit = {
    val a1 = AccountId("A1")
    val a2 = AccountId("A2")
    val t1 = TransferId(1)
    val t2 = TransferId(2)

    try {
      val w1 = EventWorld().
        send(a1, Open(50)).world

      val w2 = EventWorld().
        send(a2, Open(80)).world

      val w3 = w2 merge w1

      val w4 = w3.
        send(t1, Book(a1, a2, 30)).
        world

      val w5 = w3.
        send(t2, Book(a1, a2, 20)).
        world

      val w6 = w5 rebase w4

      println(w6.events.reverseIterator.map(_.prettyPrint).mkString("\n"))
      println(w6.obj(a1).value.balance) // 20.0
      println(w6.obj(a2).value.balance) // 110.0
      //println(w4.read(t1)) // 110.0      */
    }
    catch {
      case e: WorldErr => println(e)
    }
  }
}
