package net.manikin.example.bank

object Account {
  import net.manikin.core.state.StateObject._
  import IBAN._

  case class Id  (iban: IBAN) extends StateId[Data] { def initData = Data() }
  case class Data(balance: Long = 0) // in cents

  trait Msg extends StateMessage[Id, Data, Unit]

  case class Open(initial: Long) extends Msg {
    def nst = { case "Initial" => "Opened" }
    def pre = initial > 0
    def apl = data.copy(balance = initial)
    def eff = { }
    def pst = data.balance == initial
  }

  case class ReOpen() extends Msg {
    def nst = { case "Closed" => "Opened" }
    def pre = true
    def apl = data
    def eff = { }
    def pst = true
  }

  case class Close() extends Msg {
    def nst = { case "Opened" => "Closed" }
    def pre = true
    def apl = data
    def eff = { }
    def pst = true
  }

  case class Withdraw(amount: Long) extends Msg {
    def nst = { case "Opened" => "Opened" }
    def pre = amount > 0 && data.balance >= amount
    def apl = data.copy(balance = data.balance - amount)
    def eff = { }
    def pst = data.balance == old_data.balance - amount
  }

  case class Deposit(amount: Long) extends Msg {
    def nst = { case "Opened" => "Opened" }
    def pre = amount > 0
    def apl = data.copy(balance = data.balance + amount)
    def eff = { }
    def pst = data.balance == old_data.balance + amount
  }
}
