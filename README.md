![Manikin](docs/manikin.jpg)
# Manikin
Manikin is an embedded Scala Domain Specific Language (DSL) that implements Transactional Objects - Objects that participate and interact in the scope of Worlds.
Manikin guards Object states with pre- and post Conditions, while tracking all stateful Effects and dispatched Messages.

Manikin is heavily inspired by the [Eiffel](https://www.eiffel.com) programming language, [Software Transactional Memory](https://en.wikipedia.org/wiki/Software_transactional_memory) and [Worlds](http://www.vpri.org/pdf/tr2011001_final_worlds.pdf) that have similar goals.

### Message dispatch through Worlds
Messages are dispatched via Worlds which are then functionally updated and passed through after each (nested) dispatch.
Because Worlds keep track of all intermediate and previous Object states, it is very easy to rollback state in case of failure, or to retry Transactions after conflicts. 

### Concurrent and Distributed
Manikin can also be configured to run on top of multi-threaded, concurrent or distributed Worlds - backed by databases such as [CockroachDB](https://www.cockroachlabs.com) - with strong [Serializability](https://en.wikipedia.org/wiki/Serializability) guarantees.  
                                                           
### Syntax and Types
You can succinctly specify Objects, Identities, Messages, Conditions and Effects with Manikin *and* statically type them (as Manikin piggybacks on Scala's advanced typed system). 
Additionally, Manikin reduces the amount of boilerplate code, WITHOUT any use of Scala's more advanced features such as implicits. 

Here is a simple Bank Transfer example, written in the Manikin DSL:

```scala
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
```

```scala
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
```

```scala
package net.manikin.example.bank

object SimpleTransfer {
  import net.manikin.world.SimpleWorld._

  def main(args: Array[String]): Unit = {
    val a1 = Account.Id("A1")
    val a2 = Account.Id("A2")
    val t1 = Transfer.Id(1)

    val result = SimpleWorld().
      send(a1, Account.Open(50)).
      send(a2, Account.Open(80)).
      send(t1, Transfer.Book(a1, a2, 30))

    println(result.obj(a1).value) // SObject(Opened, Account(20.0))
    println(result.obj(a2).value) // SObject(Opened, Account(110.0))
  }
}
```
