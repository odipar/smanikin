package net.manikan.core.example

// Plain vanilla Account (no annotations)
object Account {
  import net.manikan.core.asm.AbstractStateMachine._
  import net.manikan.core.example.IBAN.IBAN

  case class Id  (iban: IBAN) extends StateID[Data] { def initData = Data() }
  case class Data(balance: Double = 0.0)

  trait Trs extends StateTransition[Data] {
    def balance = data().balance
    def prev_balance = data.previous.balance
  }

  case class Open(initial: Double) extends Trs {
    def nst = Map("Initial" -> "Opened")
    def pre = initial > 0
    def ap2 = data() = data().copy(balance = initial)
    def pst = balance == initial
  }
  
  case class Withdraw(amount: Double) extends Trs {
    def nst = Map("Opened" -> "Opened")
    def pre = amount > 0.0 && balance > amount
    def ap2 = data() = data().copy(balance = balance - amount)
    def pst = balance == prev_balance - amount
  }
                                                                               
  case class Deposit(amount: Double) extends Trs {
    def nst = Map("Opened" -> "Opened")
    def pre = amount > 0
    def ap2 = data() = data().copy(balance = balance + amount)
    def pst = balance == prev_balance + amount
  }

  case class Close() extends Trs {
    def nst = Map("Opened" -> "Closed")
    def pre = true
    def ap2 = { }
    def pst = true
  } 
}
