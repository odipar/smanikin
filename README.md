# Manikin
Manikin is an embedded Scala DSL that can be used to implement Transactional Objects - Objects that live in Transactions.
Manikin's most prominent feature is the use of pre- and post-conditions during Message dispatch, so that the state of an Object can be guarded and more easily model checked.

Manikin is inspired by the [Eiffel](https://www.eiffel.com) programming language and [Software Transactional Memory](https://en.wikipedia.org/wiki/Software_transactional_memory).

### Message dispatch through Contexts
Message dispatch is performed via Transactional Contexts that are updated and passed through after each (nested) dispatch.
Each time a Context is updated, a new version is created, for instance when an Object is written to a Context.

With Contexts it is easier to do rollbacks or analyses on failures, because Manikin keeps a full trace of all intermediate Object states. 
Next to that, Manikan can also be configured to run on top of multi-threaded, concurrent or distributed Transactional Contexts, with very strong transactional guarantees (fully Serializable).  
                                                           
### Syntax and types
A lot of Scala (implicit) trickery is used to reduce the amount of boilerplate to a minimum. 
The goal of Manikin is to be able to specify Objects and Messages as succinctly as possible while still being *statically* typed (as Manikin piggybacks on Scala's advanced typed system). 

Here is an example:
```scala
object Account {
  import net.manikin.core.asm.TransactionalObject._
  import net.manikin.core.example.IBAN.IBAN

  case class Id  (iban: IBAN) extends StateId[Data] { def initData = Data() }
  case class Data(balance: Double = 0.0)

  trait Msg extends STMessage[Data, Unit] {
    def balance =       data().balance
    def prev_balance =  data.prev.balance
  }

  case class Open(initial: Double) extends Msg {
    def nst =   { case "Initial" => "Opened" }
    def pre =   initial > 0
    def apl =   data() = data().copy(balance = initial)
    def pst =   balance == initial
  }

  case class Withdraw(amount: Double) extends Msg {
    def nst =   { case "Opened" => "Opened" }
    def pre =   amount > 0.0 && balance > amount
    def apl =   data() = data().copy(balance = balance - amount)
    def pst =   balance == prev_balance - amount
  }

  case class Deposit(amount: Double) extends Msg {
    def nst =   { case "Opened" => "Opened" }
    def pre =   amount > 0
    def apl =   data() = data().copy(balance = balance + amount)
    def pst =   balance == prev_balance + amount
  }
}
```
```scala
// Plain vanilla Transaction (no annotations)
object Transaction {
  import net.manikin.core.asm.TransactionalObject._
  import net.manikin.core.example.Account._

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
```
