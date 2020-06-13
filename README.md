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

Here is a simple Bank transaction example:
```scala
object Main {
  import net.manikin.core.TransactionalObject._
  import net.manikin.core.DefaultContext._
  import IBAN._

  def main(args: Array[String]): Unit = {
    implicit val c: Context = new DefaultContext()

    val a1 = Account.Id(iban = IBAN("A1"))
    val a2 = Account.Id(iban = IBAN("A2"))
    val t1 = Transaction.Id(id = 1)
    val t2 = Transaction.Id(id = 2)

    a1 <~ Account.Open(initial = 80.0)
    a2 <~ Account.Open(initial = 120.0)
    t1 <~ Transaction.Create(from = a1, to = a2, amount = 30.0)
    t1 <~ Transaction.Commit()
    t2 <~ Transaction.Create(from = a1, to = a2, amount = 20.0)
    t2 <~ Transaction.Commit()

    println("a1: " + c(a1)) // a1: State(Data(30.0),Opened)
    println("a2: " + c(a2)) // a2: State(Data(170.0),Opened)
    println("t1: " + c(t1)) // t1: State(Data(Id(IBAN(A1)),Id(IBAN(A2)),30.0),Committed)
    println("t2: " + c(t2)) // t2: State(Data(Id(IBAN(A1)),Id(IBAN(A2)),20.0),Committed)
  }
}
```
```scala
object Account {
  import net.manikin.core.StateMachineObject._
  import IBAN._
  
  case class Id  (iban: IBAN) extends StateId[Data] { def initData = Data() }
  case class Data(balance: Double = 0.0)

  trait Msg extends StateMessage[Data, Id, Unit] {
    def balance =       dataId().balance
    def prev_balance =  dataId.prev.balance
  }

  case class Open(initial: Double) extends Msg {
    def nst =   { case "Initial" => "Opened" }
    def pre =   initial > 0
    def apl =   dataId() = dataId().copy(balance = initial)
    def pst =   balance == initial
  }

  case class Withdraw(amount: Double) extends Msg {
    def nst =   { case "Opened" => "Opened" }
    def pre =   amount > 0.0 && balance > amount
    def apl =   dataId() = dataId().copy(balance = balance - amount)
    def pst =   balance == prev_balance - amount
  }

  case class Deposit(amount: Double) extends Msg {
    def nst =   { case "Opened" => "Opened" }
    def pre =   amount > 0
    def apl =   dataId() = dataId().copy(balance = balance + amount)
    def pst =   balance == prev_balance + amount
  }
}
```
```scala
object Transaction {
  import net.manikin.core.StateMachineObject._
  import Account._
  
  case class Id  (id: Long) extends StateId[Data] { def initData = Data() }
  case class Data(from: Account.Id = null, to: Account.Id = null, amount: Double = 0.0)

  trait Msg extends StateMessage[Data, Id, Unit]

  case class Create(from: Account.Id, to: Account.Id, amount: Double) extends Msg {
    def nst =   { case "Initial" => "Created" }
    def pre =   from().state == "Opened" && to().state == "Opened"
    def apl =   dataId() = dataId().copy(from = from, to = to, amount = amount)
    def pst =   dataId().from == from && dataId().to == to && dataId().amount == amount
  }

  case class Commit() extends Msg {
    def amt =   dataId().amount
    def from =  dataId().from
    def to =    dataId().to

    def nst =   { case "Created" => "Committed" }
    def pre =   true
    def apl =   { dataId().from <~ Withdraw(amt) ; to <~ Deposit(amt) }
    def pst =   from.prev.data.balance + to.prev.data.balance == from().data.balance + to().data.balance
  }        
}
``` 
