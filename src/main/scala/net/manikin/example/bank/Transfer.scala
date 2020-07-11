package net.manikin.example.bank

object Transfer {
  import net.manikin.core.state.StateObject._

  case class Id  (id: Long) extends StateId[Data] { def initData = Data() }
  case class Data(from: Account.Id = null, to: Account.Id = null, amount: Long = 0)

  trait Msg extends StateMessage[Id, Data, Unit]

  case class Book(from: Account.Id, to: Account.Id, amount: Long) extends Msg {
    def nst = { case "Initial" => "Booked" }
    def pre = amount > 0 && from != to
    def apl = data.copy(from = from, to = to, amount = amount)
    def eff = { from ! Account.Withdraw(amount) ; to ! Account.Deposit(amount) }
    def pst = from.old_data.balance + to.old_data.balance == from.data.balance + to.data.balance
  }
}
