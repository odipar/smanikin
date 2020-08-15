# Manikin
Manikin is an embedded Scala Domain Specific Language (DSL) that implements Transactional Objects - Objects that participate and interact in the scope of Worlds.
Manikin guards Object states with pre- and post Conditions, while tracking all stateful Effects and dispatched Messages.

Manikin is heavily inspired by the [Eiffel](https://www.eiffel.com) programming language, [Software Transactional Memory](https://en.wikipedia.org/wiki/Software_transactional_memory) and [Worlds](http://www.vpri.org/pdf/tr2011001_final_worlds.pdf) that have similar goals.

### Message dispatch through Contexts
Messages are dispatched via Worlds which are then functionally updated and passed through after each (nested) dispatch.
Because Worlds keep track of all intermediate and previous Object states, it is very easy to rollback state in case of failure, or to retry Transactions after conflicts. 

### Distributed Transactions
Manikin can also be configured to run on top of multi-threaded, concurrent or distributed Worlds - backed by databases such as [CockroachDB](https://www.cockroachlabs.com) - with strong [Serializability](https://en.wikipedia.org/wiki/Serializability) guarantees.  
                                                           
### Syntax and types
You can succinctly specify Objects, Messages, Conditions and Effects with Manikin *and* statically type them (as Manikin piggybacks on Scala's advanced typed system). 
Additionally, Manikin reduces the amount of boilerplate code, by minimal use of Scala's more advanced features such as implicits. 

Here is a simple Bank Transfer example, written in the Manikin DSL:
```scala
object SimpleTransfer {
  import net.manikin.core.context.Eventworld._
  import IBAN._
  import scala.language.implicitConversions

  def main(args: Array[String]): Unit = {
    implicit val ctx = new EventWorld()

    val a1 = Account.Id(iban = IBAN("A1"))
    val a2 = Account.Id(iban = IBAN("A2"))
    val t1 = Transfer.Id(id = 1)
    val t2 = Transfer.Id(id = 2)

    a1 ! Account.Open(initial = 80)
    a2 ! Account.Open(initial = 120)
    t1 ! Transfer.Book(from = a1, to = a2, amount = 30)
    t2 ! Transfer.Book(from = a1, to = a2, amount = 40)

    println("a1: " + ctx(a1)) // a1: StateObject(Data(10.0),Opened)
    println("a2: " + ctx(a2)) // a2: StateObject(Data(190.0),Opened)
    println("t1: " + ctx(t1)) // t1: StateObject(Data(Id(IBAN(A1)),Id(IBAN(A2)),30.0),Booked)
    println("t2: " + ctx(t2)) // t1: StateObject(Data(Id(IBAN(A1)),Id(IBAN(A2)),40.0),Booked)
  }
}
```
```scala
object Account {
  import net.manikin.core.state.StateObject._
  import IBAN._

  case class Id  (iban: IBAN) extends StateId[State] { def initData = State() }
  case class State(balance: Long = 0) // in cents

  trait Msg extends StateMessage[Id, State, Unit]

  case class Open(initial: Long) extends Msg {
    def nst = { case "Initial" => "Opened" }
    def pre = initial > 0
    def apl = state.copy(balance = initial)
    def eff = { }
    def pst = state.balance == initial
  }

  case class ReOpen() extends Msg {
    def nst = { case "Closed" => "Opened" }
    def pre = true
    def apl = state
    def eff = { }
    def pst = true
  }

  case class Close() extends Msg {
    def nst = { case "Opened" => "Closed" }
    def pre = true
    def apl = state
    def eff = { }
    def pst = true
  }

  case class Withdraw(amount: Long) extends Msg {
    def nst = { case "Opened" => "Opened" }
    def pre = amount > 0 && state.balance >= amount
    def apl = state.copy(balance = state.balance - amount)
    def eff = { }
    def pst = state.balance == old_state.balance - amount
  }

  case class Deposit(amount: Long) extends Msg {
    def nst = { case "Opened" => "Opened" }
    def pre = amount > 0
    def apl = state.copy(balance = state.balance + amount)
    def eff = { }
    def pst = state.balance == old_state.balance + amount
  }
}
```
```scala
bject Transfer {
  import net.manikin.core.state.StateObject._

  case class Id  (id: Long) extends StateId[State] { def initData = State() }
  case class State(from: Account.Id = null, to: Account.Id = null, amount: Long = 0)

  trait Msg extends StateMessage[Id, State, Unit]

  case class Book(from: Account.Id, to: Account.Id, amount: Long) extends Msg {
    def nst = { case "Initial" => "Booked" }
    def pre = amount > 0 && from != to
    def apl = state.copy(from = from, to = to, amount = amount)
    def eff = { from ! Account.Withdraw(amount) ; to ! Account.Deposit(amount) }
    def pst = from.old_state.balance + to.old_state.balance == from.state.balance + to.state.balance
  }
}
```
