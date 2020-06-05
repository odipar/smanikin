# Manikin
Manikin is an embedded Scala DSL that can be used to implement Abstract State Machines (ASMs).
Its most prominent feature is the use of pre- and post-conditions so that the state of ASMs can be guarded and more easily model checked.

Manikin is inspired by the [Rebel](https://github.com/cwi-swat/rebel) DSL (with some important deviations).
Another source of inspiration is the [Eiffel](https://www.eiffel.com) programming language.

(It can be argued that Rebel is a restricted version of Eiffel, as Rebel does not allow *conditional* transitions)

### Updates through Contexts
Evaluation is done via a Context object, which is updated and passed through after each evaluation step.
Every time a Context is updated, a new version is created, for instance when an ASM is written to a Context.

As a result, it will be easier to do analyses on failures, because Manikin keeps a full trace of all intermediate Contexts. 

### Syntax and types
A lot of Scala (implicit) trickery is used to reduce the amount of boilerplate syntax to a minimum. 
The goal of Manikin is to be able to specify an ASM as succinctly as possible while being *statically* typed (as Thunk piggybacks on Scala's advanced typed system). 

Here is an example:
```scala
// Plain vanilla Account (no annotations)
object Account {
  import net.manikin.core.asm.AbstractStateMachine._
  import net.manikin.core.example.IBAN.IBAN

  case class Id  (iban: IBAN) extends StateId[Data] { def initData = Data() }
  case class Data(balance: Double = 0.0)

  trait Trs extends StateTransition[Data] {
    def balance =       data().balance
    def prev_balance =  data.prev.balance
  }

  case class Open(initial: Double) extends Trs {
    def nst =   { case "Initial" => "Opened" }
    def pre =   initial > 0
    def apl =   data() = data().copy(balance = initial)
    def pst =   balance == initial
  }
  
  case class Withdraw(amount: Double) extends Trs {
    def nst =   { case "Opened" => "Opened" }
    def pre =   amount > 0.0 && balance > amount
    def apl =   data() = data().copy(balance = balance - amount)
    def pst =   balance == prev_balance - amount
  }
                                                                               
  case class Deposit(amount: Double) extends Trs {
    def nst =   { case "Opened" => "Opened" }
    def pre =   amount > 0
    def apl =   data() = data().copy(balance = balance + amount)
    def pst =   balance == prev_balance + amount
  }
}
```
```scala
object Transaction {
  import net.manikin.core.asm.AbstractStateMachine._
  import net.manikin.core.example.Account._

  case class Id  (id: Long) extends StateId[Data] { def initData = Data() }
  case class Data(from: Account.Id = null, to: Account.Id = null, amount: Double = 0.0)

  trait Trs extends StateTransition[Data]

  case class Create(from: Account.Id, to: Account.Id, amount: Double) extends Trs {
    def nst =   { case "Initial" => "Created" }
    def pre =   from().state == "Opened" && to().state == "Opened"
    def apl =   data() = data().copy(from = from, to = to, amount = amount)
    def pst =   data().from == from && data().to == to && data().amount == amount
  }

  case class Commit() extends Trs {
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
