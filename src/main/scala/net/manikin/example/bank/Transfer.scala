package net.manikin.example.bank

object Transfer {
  import net.manikin.core.state.StateObject._

  case class Id  (id: Long) extends StateId[State] { def initState = State() }
  case class State(from: Account.Id = null, to: Account.Id = null, amount: Long = 0)

  trait Msg extends StateMessage[Id, State, Unit]

  case class Book(from: Account.Id, to: Account.Id, amount: Long) extends Msg {
    def nst = { case "Initial" => "Booked" }
    def pre = amount > 0 && from != to
    def apl = state.copy(from = from, to = to, amount = amount)
    def eff = { from ! Account.Withdraw(amount) ; to ! Account.Deposit(amount) }
    def pst = from.old_state.balance + to.old_state.balance == from.state.balance + to.state.balance
  }
}
