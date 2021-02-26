package net.manikin.example.bank

object Account {
  import net.manikin.core.Core._
  import net.manikin.message.StateObject._

  case class Account(balance: Double = 0.0)
  trait Msg[W <: World[W]] extends SMsg[W, Id, Account, Unit]

  case class Id(IBAN: String) extends SId[Account] {
    def ini = Account()
  }

  case class Open[W <: World[W]](init: Double) extends Msg[W] {
    def nst = { case "Initial" => "Opened" }
    def pre = init > 0.0
    def app = obj.copy(balance = init)
    def eff = ()
    def pst = obj.balance == init
  }

  case class Withdraw[W <: World[W]](amount: Double) extends Msg[W] {
    def nst = { case "Opened" => "Opened" }
    def pre = amount > 0.0 && obj.balance >= amount
    def app = obj.copy(balance = obj.balance - amount)
    def eff = ()
    def pst = obj.balance == old.balance - amount
  }

  case class Deposit[W <: World[W]](amount: Double) extends Msg[W] {
    def nst = { case "Opened" => "Opened" }
    def pre = amount > 0.0
    def app = obj.copy(balance = obj.balance + amount)
    def eff = ()
    def pst = obj.balance == old.balance + amount
  }
}
