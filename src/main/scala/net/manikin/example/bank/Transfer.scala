package net.manikin.example.bank

object  Transfer {
  import net.manikin.core.state.StateObject._
  import Account._

  case class TransferId  (id: Long) extends StateId[TransferData] { def initData = TransferData() }
  case class TransferData(from: Account.AccountId = null, to: Account.AccountId = null, amount: Double = 0.0)

  trait TransferMessage[+R] extends StateMessage[TransferData, TransferId, R]

  case class Create(from: Account.AccountId, to: Account.AccountId, amount: Double) extends TransferMessage[Unit] {
    def nst = { case "Initial" => "Created" }
    def pre = { amount > 0 && from != to }
    def apl = { data.copy(from = from, to = to, amount = amount) }
    def eff = { }
    def pst = { data.from == from && data.to == to && data.amount == amount }
  }

  case class Book() extends TransferMessage[Unit] {
    def nst = { case "Created" => "Booked" }
    def pre = { true }
    def apl = { data }
    def eff = { data.from ! Withdraw(data.amount) ; data.to ! Deposit(data.amount)  }
    def pst = { data.from.old_data.balance + data.to.old_data.balance == data.from.data.balance + data.to.data.balance }
  }
}
