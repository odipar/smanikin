package net.manikin.core.example

// Plain vanilla Account (no annotations)
object Account {
  import net.manikin.core.asm.AbstractStateMachine._
  import net.manikin.core.example.IBAN.IBAN

  case class Id  (iban: IBAN) extends StateID[Data] { def initData = Data() }
  case class Data(balance: Double = 0.0)

  trait Trs extends StateTransition[Data] {
    def balance =       data().balance
    def prev_balance =  data.prev.balance
  }

  case class Open(initial: Double) extends Trs {
    def nst =   Map("Initial" -> "Opened")
    def pre =   initial > 0
    def apl =   data() = data().copy(balance = initial)
    def pst =   balance == initial
  }
  
  case class Withdraw(amount: Double) extends Trs {
    def nst =   Map("Opened" -> "Opened")
    def pre =   amount > 0.0 && balance > amount
    def apl =   data() = data().copy(balance = balance - amount)
    def pst =   balance == prev_balance - amount
  }
                                                                               
  case class Deposit(amount: Double) extends Trs {
    def nst =   Map("Opened" -> "Opened")
    def pre =   amount > 0
    def apl =   data() = data().copy(balance = balance + amount)
    def pst =   balance == prev_balance + amount
  }
}
