package net.manikin.example.bank

object AccountHolder {
  import net.manikin.core.state.StateObject._
  import net.manikin.core.context.StoreWorld.StoreWorld
  import IBAN._

  case class AccountId(iban: IBAN) extends StateId[AccountState] { def initState = AccountState() }
  case class AccountState(balance: Long = 0, minimum: Long = 0, holder: HolderId = null)

  case class HolderId(id: String) extends StateId[HolderData] {
    def initState = HolderData()
  }

  case class HolderData(accounts: Set[AccountId] = Set())

  trait AccountMessage extends StateMessage[AccountId, AccountState, Unit] {
    def accounts = state.holder.state.accounts
    def holderInvariant = accounts.forall(_.state.holder == state.holder) && accounts.map(_.state.balance).sum >= 0

    def invariant = state.balance >= state.minimum && holderInvariant
  }
  
  trait HolderMessage extends StateMessage[HolderId, HolderData, Unit]
  
  case class TransferId(id: Long) extends StateId[TransferState] { def initState = TransferState() }
  case class TransferState(from: AccountId = null, to: AccountId = null, amount: Long = 0)

  trait TransferMessage extends StateMessage[TransferId, TransferState, Unit] {
    def amount = state.amount
    def from   = state.from
    def to     = state.to
  }

  case class Create(_from: AccountId, _to: AccountId, _amount: Long) extends TransferMessage {
    def nst = { case "Initial" => "Created" }
    def pre = _amount > 0 && _from != _to
    def apl = state.copy(from = _from, to = _to, amount = _amount)
    def eff = { }
    def pst = from == _from && to == _to && amount == _amount
  }

  case class Book() extends TransferMessage {
    def nst = { case "Created" => "Booked" }
    def pre = true
    def apl = state
    def eff = { from ! Withdraw(amount) ; to ! Deposit(amount) }
    def pst = from.old_state.balance + to.old_state.balance == from.state.balance + to.state.balance
  }
  
  case class Open(initial: Long, minimum: Long, holder: HolderId) extends AccountMessage {
    def nst = { case "Initial" => "Opened" }
    def pre = initial > 0
    def apl = state.copy(balance = initial, minimum = minimum, holder = holder)
    def eff = { holder ! AddAccount(self) }
    def pst = invariant && state.balance == initial
  }

  case class Withdraw(amount: Long) extends AccountMessage {
    def nst = { case "Opened" => "Opened" }
    def pre = invariant && amount > 0
    def apl = state.copy(balance = state.balance - amount)
    def eff = { }
    def pst = invariant && state.balance == old_state.balance - amount
  }

  case class Deposit(amount: Long) extends AccountMessage {
    def nst = { case "Opened" => "Opened" }
    def pre = invariant && amount > 0
    def apl = state.copy(balance = state.balance + amount)
    def eff = { }
    def pst = invariant && state.balance == old_state.balance + amount
  }

  case class AddAccount(account: AccountId) extends HolderMessage {
    def nst = { case x => x }
    def pre = account.state.holder == self
    def apl = state.copy(accounts = state.accounts + account)
    def eff = { }
    def pst = state.accounts.contains(account)
  }

  def main(args: Array[String]): Unit = {
    implicit val ctx = new StoreWorld()

    val h1 = HolderId("h1")
    val h2 = HolderId("h2")
    val a1 = AccountId(iban = IBAN("A1"))
    val a2 = AccountId(iban = IBAN("A2"))
    val a3 = AccountId(iban = IBAN("A3"))
    val a4 = AccountId(iban = IBAN("A4"))
    val t1 = TransferId(id = 1)
    val t2 = TransferId(id = 2)

    a1 ! Open(initial = 60, minimum = -20, holder = h1)
    a2 ! Open(initial = 10, minimum = 0, holder = h1)
    a3 ! Open(initial = 10, minimum = -10, holder = h2)
    a4 ! Open(initial = 20, minimum = -10, holder = h2)

    t1 ! Create(_from = a1, _to = a3, _amount = 30)
    t2 ! Create(_from = a1, _to = a4, _amount = 40)
    t2 ! Book()
    t1 ! Book()

    //println("sends: " + ctx.sends)

    println("h1: " + ctx(h1))
    println("h2: " + ctx(h2))
    println("a1: " + ctx(a1))
    println("a2: " + ctx(a2))
    println("a3: " + ctx(a3))
    println("a4: " + ctx(a4))
    println("t1: " + ctx(t1))
    println("t2: " + ctx(t2))
  }
}
