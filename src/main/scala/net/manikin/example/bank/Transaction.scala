package net.manikin.example.bank

import net.manikin.example.Account

// Plain vanilla Transaction (no annotations)
object Transaction {
  import net.manikin.core.TransactionalObject._

  case class Id  (id: Long) extends StateId[Data] { def initData = Data() }
  case class Data(from: Account.Id = null, to: Account.Id = null, amount: Double = 0.0)

  trait Msg extends STMessage[Data, Unit]

  case class Create(from: Account.Id, to: Account.Id, amount: Double) extends Msg {
    def nst =   { case "Initial" => "Created" }
    def pre =   from().state == "Opened" && to().state == "Opened"
    def apl =   data() = data().copy(from = from, to = to, amount = amount)
    def pst =   data().from == from && data().to == to && data().amount == amount
  }

  case class Commit() extends Msg {
    def amt =   data().amount
    def from =  data().from
    def to =    data().to

    def nst =   { case "Created" => "Committed" }
    def pre =   true
    def apl =   { Withdraw(amt) --> data().from ; Deposit(amt) --> to }
    def pst =   from.prev.data.balance + to.prev.data.balance == from().data.balance + to().data.balance
  }
}
