package net.manikin.example.bank

object Account {
  import net.manikin.core.StateMachineObject._
  import IBAN._
  
  case class Id  (iban: IBAN) extends StateId[Data] { def initData = Data() }
  case class Data(balance: Double = 0.0)

  trait Msg extends StateMessage[Data, Id, Unit] {
    def balance =       data().balance
    def prev_balance =  data.prev.balance
  }

  case class Open(initial: Double) extends Msg {
    def nst =   { case "Initial" => "Opened" }
    def pre =   initial > 0
    def apl =   data() = data().copy(balance = initial)
    def pst =   balance == initial
  }

  case class Withdraw(amount: Double) extends Msg {
    def nst =   { case "Opened" => "Opened" }
    def pre =   amount > 0.0 && balance > amount
    def apl =   data() = data().copy(balance = balance - amount)
    def pst =   balance == prev_balance - amount
  }

  case class Deposit(amount: Double) extends Msg {
    def nst =   { case "Opened" => "Opened" }
    def pre =   amount > 0
    def apl =   data() = data().copy(balance = balance + amount)
    def pst =   balance == prev_balance + amount
  }
}
