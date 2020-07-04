package net.manikin.example.bank

object Transfer {
  import net.manikin.core.state.StateObject._

  case class Id  (id: Long) extends StateId[Data] { def initData = Data() }
  case class Data(from: Account.Id = null, to: Account.Id = null, amount: Long = 0)

  trait Msg extends StateMessage[Data, Id, Unit] {
    def amount = data.amount
    def from   = data.from
    def to     = data.to
  }

  case class Create(_from: Account.Id, _to: Account.Id, _amount: Long) extends Msg {
    def nst = { case "Initial" => "Created" }
    def pre = _amount > 0 && _from != _to
    def apl = data.copy(from = _from, to = _to, amount = _amount)
    def eff = { }
    def pst = from == _from && to == _to && amount == _amount
  }

  case class Book() extends Msg {
    def nst = { case "Created" => "Booked" }
    def pre = true
    def apl = data
    def eff = { from ! Account.Withdraw(amount) ; to ! Account.Deposit(amount) }
    def pst = from.old_data.balance + to.old_data.balance == from.data.balance + to.data.balance
  }
}
