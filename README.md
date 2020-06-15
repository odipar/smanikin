# Manikin
Manikin is an embedded Scala DSL that can be used to implement Transactional Objects - Objects that are controlled with Transactions.
Its most prominent feature is the use of pre- and post-conditions during Message dispatch, so that Object states can be guarded and more easily model checked.

Manikin is inspired by the [Eiffel](https://www.eiffel.com) programming language and [Software Transactional Memory](https://en.wikipedia.org/wiki/Software_transactional_memory).

### Message dispatch through Contexts
Message dispatch is performed via Transactional Contexts that are updated and passed through after each (nested) dispatch.
New versions are created each time a Context is updated, for instance when an Object is written to a Context.

With the aid of Contexts, it is easier to do rollbacks or analyses on failures, because it can keep track of all intermediate Object states.
Next to that, Manikin can also be configured to run on top of multi-threaded, concurrent or distributed Transactional Contexts, with (optionally) very strong Transactional guarantees (fully Serializable).  
                                                           
### Syntax and types
A lot of Scala (implicit) trickery is used to reduce the amount of boilerplate to a minimum. 
The goal of Manikin is to be able to specify Objects and Messages as succinctly as possible while still being *statically* typed (as Manikin piggybacks on Scala's advanced typed system). 

Here is a simple Bank transfer example:
```scala
object Account {
  import net.manikin.core.state.StateObject._
  import IBAN._
  
  case class Id  (iban: IBAN) extends StateId[Data] { def initData = Data() }
  case class Data(balance: Double = 0.0)

  trait Msg extends StateMessage[Data, Id, Unit]

  case class Open(initial: Double) extends Msg {
    def nst = { case "Initial" => "Opened" }
    def pre = initial > 0
    def apl = data.copy(balance = initial)
    def eff = { }
    def pst = data.balance == initial
  }

  case class Withdraw(amount: Double) extends Msg {
    def nst = { case "Opened" => "Opened" }
    def pre = amount > 0 && data.balance > amount
    def apl = data.copy(balance = data.balance - amount)
    def eff = { }
    def pst = data.balance == old_data.balance - amount
  }

  case class Deposit(amount: Double) extends Msg {
    def nst = { case "Opened" => "Opened" }
    def pre = amount > 0
    def apl = data.copy(balance = data.balance + amount)
    def eff = { }
    def pst = data.balance == old_data.balance + amount
  }
}
```
```scala
object Transfer {
  import net.manikin.core.state.StateObject._
  import Account._

  case class Id      (id: Long) extends StateId[Transfer] { def initData = Transfer() }
  case class Transfer(from: Account.Id = null, to: Account.Id = null, amount: Double = 0.0)

  trait Msg extends StateMessage[Transfer, Id, Unit] {
    def amount = data.amount
    def from   = data.from
    def to     = data.to
  }

  case class Create(_from: Account.Id, _to: Account.Id, _amount: Double) extends Msg {
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
    def eff = { from ! Withdraw(data.amount) ; to ! Deposit(data.amount) }
    def pst = from.old_data.balance + to.old_data.balance == from.data.balance + to.data.balance
  }
}
```  
```scala
object SimpleTransfer {
  import net.manikin.core.TransObject._
  import net.manikin.core.context.DefaultContext._
  import IBAN._
  import scala.language.implicitConversions

  def main(args: Array[String]): Unit = {
    implicit val ctx = DefaultContext()

    val a1 = Account.Id(iban = IBAN("A1"))
    val a2 = Account.Id(iban = IBAN("A2"))
    val t1 = Transfer.Id(id = 1)
    val t2 = Transfer.Id(id = 2)

    a1 ! Account.Open(initial = 80.0)
    a2 ! Account.Open(initial = 120.0)
    t1 ! Transfer.Create(_from = a1, _to = a2, _amount = 30.0)
    t2 ! Transfer.Create(_from = a1, _to = a2, _amount = 40.0)
    t2 ! Transfer.Book()
    t1 ! Transfer.Book()

    println("a1: " + ctx(a1).obj) // a1: StateObject(Data(10.0),Opened)
    println("a2: " + ctx(a2).obj) // a2: StateObject(Data(190.0),Opened)
    println("t1: " + ctx(t1).obj) // t1: StateObject(Data(Id(IBAN(A1)),Id(IBAN(A2)),30.0),Booked)
    println("t2: " + ctx(t2).obj) // t1: StateObject(Data(Id(IBAN(A1)),Id(IBAN(A2)),40.0),Booked)
  }
}
```
