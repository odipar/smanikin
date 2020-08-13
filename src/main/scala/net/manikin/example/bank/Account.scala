package net.manikin.example.bank

object Account {
  import net.manikin.core.state.StateObject._
  import IBAN._

  case class Id  (iban: IBAN) extends StateId[State] { def initData = State() }
  case class State(balance: Long = 0) // in cents

  trait Msg extends StateMessage[Id, State, Unit]

  case class Open(initial: Long) extends Msg {
    def nst = { case "Initial" => "Opened" }
    def pre = initial > 0
    def apl = state.copy(balance = initial)
    def eff = { }
    def pst = state.balance == initial
  }

  case class ReOpen() extends Msg {
    def nst = { case "Closed" => "Opened" }
    def pre = true
    def apl = state
    def eff = { }
    def pst = true
  }

  case class Close() extends Msg {
    def nst = { case "Opened" => "Closed" }
    def pre = true
    def apl = state
    def eff = { }
    def pst = true
  }

  case class Withdraw(amount: Long) extends Msg {
    def nst = { case "Opened" => "Opened" }
    def pre = amount > 0 && state.balance >= amount
    def apl = state.copy(balance = state.balance - amount)
    def eff = { }
    def pst = state.balance == old_state.balance - amount
  }

  case class Deposit(amount: Long) extends Msg {
    def nst = { case "Opened" => "Opened" }
    def pre = amount > 0
    def apl = state.copy(balance = state.balance + amount)
    def eff = { }
    def pst = state.balance == old_state.balance + amount
  }
}
