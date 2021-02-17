package net.manikin.main

object Main2 {

  trait Id[+O] {
    def ini: O
  }
  

  trait Msg[-I <: Id[O], O, +R, C <: Context[C]] {
    def pre: Self[I, O, C] => Boolean
    def app: O => O
    def eff: Self[I, O, C] => Result[R, C]
    def pst: Self[I, O, C] => Boolean
  }

  trait Context[C <: Context[C]] {
    def prev: C
    def apply[O](id: Id[O]): O
    def send[I <: Id[O], O, R](id: I, msg: Msg[I, O, R, C]): Result[R, C]
  }

  case class Self[+I <: Id[O], O, C <: Context[C]](self: I, context: C) {
    def apply(): O = context(self)
    def prev(): O = context.prev(self)
    def apply[O2](id: Id[O2]): O2 = context(id)
    def prev[O2](id: Id[O2]): O2 = context.prev(id)
    def send[I2 <: Id[O2], O2, R](id: I2, msg: Msg[I2, O2, R, C]): Result[R, C] = context.send(id, msg)
  }

  case class Result[+R, C <: Context[C]](result: R, context: C) {
    def apply(): R = result
    def apply[O2](id: Id[O2]): O2 = context(id)
    def prev[O2](id: Id[O2]): O2 = context.prev(id)
    def send[I2 <: Id[O2], O2, R2](id: I2, msg: Msg[I2, O2, R2, C]): Result[R2, C] = context.send(id, msg)
  }


  case class SimpleContext(prev: SimpleContext = null, state: Map[Id[_], _] = Map()) extends Context[SimpleContext] {
    def previous = prev
    def apply[O](id: Id[O]): O = state.getOrElse(id, id.ini).asInstanceOf[O]
    def send[I <: Id[O], O, R](id: I, msg: Msg[I, O, R, SimpleContext]): Result[R, SimpleContext] = {
      if (!msg.pre(Self(id, this))) throw sys.error("Pre-condition failed")
      else {
        val eff = msg.eff(Self(id, SimpleContext(this, state + (id -> msg.app(apply(id))))))
        if (msg.pst(Self(id, SimpleContext(this, eff.context.state)))) eff
        else throw sys.error("Post-condition failed")
      }
    }
  }

  trait SMsg[-I <: Id[O], O] extends Msg[I, O, Unit, SimpleContext]
  def none[I2 <: Id[O2], O2, C <: Context[C]] = (c: Self[I2, O2, C]) => Result[Unit, C]((), c.context)

  case class AccountId(IBAN: String) extends Id[Account] {
    def ini = Account()
  }

  case class Account(balance: Double = 0.0)

  case class Open(init: Double) extends SMsg[AccountId, Account] {
    def pre = _ => init > 0.0
    def app = _ => Account(init)
    def eff = none
    def pst = c => c().balance == init
  }

  case class Withdraw(amount: Double) extends SMsg[AccountId, Account]  {
    def pre = c => amount > 0.0 && c().balance >= amount
    def app = o => Account(o.balance - amount)
    def eff = none
    def pst = c => c().balance == c.prev().balance - amount
  }

  case class Deposit(amount: Double) extends SMsg[AccountId, Account]  {
    def pre = _ => amount > 0.0
    def app = o => Account(o.balance + amount)
    def eff = none
    def pst = c => c().balance == c.prev().balance + amount
  }

  case class TransferId(id: Long) extends Id[Transfer] {
    def ini = Transfer()
  }

  case class Transfer(from: AccountId = null, to: AccountId = null, amount: Double = 0.0)

  case class Book(from: AccountId, to: AccountId, amount: Double) extends SMsg[TransferId, Transfer] {
    def pre = _ => amount >= 0.0
    def app = _ => Transfer(from, to, amount)
    def eff = c => c.send(from, Withdraw(amount)).send(to, Deposit(amount))
    def pst = c => c(from).balance + c(to).balance == c.prev(from).balance + c.prev(to).balance
  }

  def main(args: Array[String]): Unit = {
    val c = SimpleContext()
    val a1 = AccountId("A1")
    val a2 = AccountId("A2")
    val t1 = TransferId(1)

    val c2 = c.
      send(a1, Open(50)).
      send(a2, Open(80)).
      send(t1, Book(a1, a2, 30))

    println("c2: " + c2.context.state)

  }
}
