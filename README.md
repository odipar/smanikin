# Manikin
Manikin is an embedded Scala Domain Specific Language (DSL) that implements Transactional Objects - Objects that participate and interact in the scope of Transactions.
Manikin guards Object states with pre- and post Conditions, while tracking all stateful Effects and dispatched Messages.

Manikin is heavily inspired by the [Eiffel](https://www.eiffel.com) programming language and [Software Transactional Memory](https://en.wikipedia.org/wiki/Software_transactional_memory) that have similar goals.

### Message dispatch through Contexts
Messages are dispatched via Transactional Contexts which are then functionally updated and passed through after each (nested) dispatch.
Because Contexts keep track of all intermediate and previous Object states, it is very easy to rollback state in case of failure, or to retry Transactions after conflicts. 

### Distributed Transactions
Manikin can also be configured to run on top of multi-threaded, concurrent or distributed Transactions - backed by databases such as [CockroachDB](https://www.cockroachlabs.com) - with strong [Serializability](https://en.wikipedia.org/wiki/Serializability) guarantees.  
                                                           
### Syntax and types
You can succinctly specify Objects, Messages, Conditions and Effects with Manikin *and* statically type them (as Manikin piggybacks on Scala's advanced typed system). 
Additionally, Manikin reduces the amount of boilerplate code, by minimal use of Scala's more advanced features such as implicits. 

Here is a simple Bank Transfer example, written in the Manikin DSL:
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
