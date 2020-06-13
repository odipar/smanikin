package net.manikin.example.bank

object Account {
  import net.manikin.core.StateMachineObject._
  import IBAN._
  
  case class AccountId  (iban: IBAN) extends StateId[AccountData] { def initData = AccountData() }
  case class AccountData(balance: Double = 0.0)

  trait Msg[+R] extends StateMessage[AccountData, AccountId, R]

  case class Open(initial: Double) extends Msg[Unit] {
    def nst = { case "Initial" => "Opened" }
    def pre = { initial > 0 }
    def apl = { data.copy(balance = initial) }
    def eff = { }
    def pst = { data.balance == initial }
  }

  case class Withdraw(amount: Double) extends Msg[Unit] {
    def nst = { case "Opened" => "Opened" }
    def pre = { amount > 0 && data.balance > amount }
    def apl = { data.copy(balance = data.balance - amount) }
    def eff = { }
    def pst = { data.balance == old_data.balance - amount }
  }

  case class Deposit(amount: Double) extends Msg[Unit] {
    def nst = { case "Opened" => "Opened" }
    def pre = { amount > 0 }
    def apl = { data.copy(balance = data.balance + amount)  }
    def eff = { }
    def pst = { data.balance == old_data.balance + amount }
  }
}
