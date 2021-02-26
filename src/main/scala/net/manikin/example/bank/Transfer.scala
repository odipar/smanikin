package net.manikin.example.bank

object Transfer {
  import net.manikin.core.Core._
  import net.manikin.message.StateObject._

  case class Transfer(from: Account.Id = null, to: Account.Id = null, amount: Double = 0.0)
  trait Msg[W <: World[W]] extends SMsg[W, Id, Transfer, Unit]

  case class Id(id: Long) extends SId[Transfer] {
    def ini = Transfer()
  }

  case class Book[W <: World[W]](from: Account.Id, to: Account.Id, amount: Double) extends Msg[W] {
    def nst = { case "Initial" => "Booked" }
    def pre = amount > 0.0 && from != to && state(from) == "Opened" && state(to) == "Opened"
    def app = Transfer(from, to, amount)
    def eff = { send(from, Account.Withdraw(amount)); send(to, Account.Deposit(amount)) }
    def pst = obj(from).balance + obj(to).balance == old(from).balance + old(to).balance
  }
}
