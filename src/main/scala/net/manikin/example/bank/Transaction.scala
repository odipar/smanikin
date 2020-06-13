package net.manikin.example.bank

object  Transaction {
  import net.manikin.core.StateMachineObject._
  import Account._

  case class TransactionId  (id: Long) extends StateId[TransactionData] { def initData = TransactionData() }
  case class TransactionData(from: Account.AccountId = null, to: Account.AccountId = null, amount: Double = 0.0)

  trait TransactionMessage[+R] extends StateMessage[TransactionData, TransactionId, R]

  case class Create(from: Account.AccountId, to: Account.AccountId, amount: Double) extends TransactionMessage[Unit] {
    def nst = { case "Initial" => "Created" }
    def pre = { amount > 0 && from != to }
    def apl = { data.copy(from = from, to = to, amount = amount) }
    def eff = { }
    def pst = { data.from == from && data.to == to && data.amount == amount }
  }

  case class Commit() extends TransactionMessage[Unit] {
    def nst = { case "Created" => "Committed" }
    def pre = { true }
    def apl = { data }
    def eff = { data.from ! Withdraw(data.amount) ; data.to ! Deposit(data.amount)  }
    def pst = { data.from.old_data.balance + data.to.old_data.balance == data.from.data.balance + data.to.data.balance }
  }
}
