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
  case class Transfer(from: AccountId = null, to: AccountId = null, amount: Double = 0.0, failed: Boolean = false)
  trait TransferMsg extends ScalaMessage[TransferId, Transfer, Unit]

  case class FallibleBook(from: AccountId, to: AccountId, amount: Double) extends TransferMsg {
    def scala =
      pre { amount > 0.0 && from != to }.
      app { Transfer(from, to, amount) }.
      eff {
        try   { send(self, ApplyBook(from, to, amount)) }
        catch { case e: Throwable => send(self, FailBook(e)) }
      }.
      pst { obj(from).balance + obj(to).balance == old(from).balance + old(to).balance }

    private case class ApplyBook(from: AccountId, to: AccountId, amount: Double) extends TransferMsg {
      def scala =
        pre { true }.
        app { obj }.
        eff { send(to, Deposit(amount)) ; send(from, Withdraw(amount)) }.
        pst { true }
    }

    private case class FailBook(e: Throwable) extends TransferMsg {
      def scala =
        pre { obj.from != null && obj.to != null && !obj.failed }.
        app { obj.copy(failed = true) }.
        eff { }.
        pst { obj.failed }
    }
  }

  def main(args: Array[String]): Unit = {
    val a1 = AccountId("A1")
    val a2 = AccountId("A2")
    val t1 = TransferId(1)
    val t2 = TransferId(2)

    try {

      val w1 = EventWorld().
        send(a1, Open(50)).
        send(a2, Open(80)).world

      val w2 = w1.
        send(t1, FallibleBook(a1, a2, 60)).world

      val w3 = w1.
        send(t2, FallibleBook(a2, a1, 30)).world

      val w4 = w2.rebase(w3)

      println("w2:\n" + w2)
      println("w3:\n" + w3)
      println("w4:\n" + w4)
      println(w4.obj(a1).value.balance)
      println(w4.obj(a2).value.balance)
    }
    catch {
      case e: WorldErr => println(e)
    }
  }
}
