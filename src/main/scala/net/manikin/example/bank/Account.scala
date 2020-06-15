package net.manikin.example.bank

object Account {
  import net.manikin.core.state.StateObject._
  import IBAN._
  
  case class Id  (iban: IBAN) extends StateId[Data] { def initData = Data() }
  case class Data(balance: Double = 0.0)

  trait Msg extends StateMessage[Data, Id, Unit]

  case class Open(initial: Double) extends Msg {
    def nst = { case "Initial" => "Opened" }
    def pre = initial > 0
    def apl = data.copy(balance = initial)
    def eff = { }
    def pst = data.balance == initial
  }

  case class Withdraw(amount: Double) extends Msg {
    def nst = { case "Opened" => "Opened" }
    def pre = amount > 0 && data.balance > amount
    def apl = data.copy(balance = data.balance - amount)
    def eff = { }
    def pst = data.balance == old_data.balance - amount
  }

  case class Deposit(amount: Double) extends Msg {
    def nst = { case "Opened" => "Opened" }
    def pre = amount > 0
    def apl = data.copy(balance = data.balance + amount)
    def eff = { }
    def pst = data.balance == old_data.balance + amount
  }
}
