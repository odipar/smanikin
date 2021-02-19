package net.manikin.main

object Main {

  trait Id[+O] {
    def init: O
  }

  trait Msg[I <: Id[O], O, +R, C <: Context[C]]{
    type V[+X] = CVal[X, C]

    def preCondition: V[I] => V[Boolean]
    def apply: V[I] => V[O]
    def effect: V[I] => V[R]
    def postCondition: V[I] => V[Boolean]
  }

  trait Context[C <: Context[C]] {
    type V[+X] = CVal[X, C]

    def apply[O](id: Id[O]): V[O]
    def previous[O](id: Id[O]): V[O]
    def send[I <: Id[O], O, R](id: I, msg: Msg[I, O, R, C]): V[R]
  }

  case class CVal[+R, C <: Context[C]](value: R, context: C) extends Context[C] {
    def apply[O](id: Id[O]): V[O] = context(id)
    def previous[O](id: Id[O]): V[O] = context.previous(id)
    def send[I <: Id[O], O, R2](id: I, msg: Msg[I, O, R2, C]): V[R2] = context.send(id, msg)
  }

  val cVal = new ThreadLocal[CVal[_, _]]

  trait CMsg[I <: Id[O], O, +R, C <: Context[C]] extends Msg[I, O, R, C] {
    def preCondition2: Boolean
    def apply2: O
    def effect2: R
    def postCondition2: Boolean

    def preCondition = c => s(c, preCondition2)
    def apply = c => s(c, apply2)
    def effect = c => s(c, effect2)
    def postCondition = c => s(c, postCondition2)

    def context: C = get().context
    def self: I = get().value
    def obj: O = ss(context(self))
    def prev: O = ss(context.previous(self))

    def get(): V[I] = cVal.get().asInstanceOf[V[I]]
    def set(c: V[I]): Unit = cVal.set(c)

    def prev[O2, R2](id: Id[O2]): O2 = ss(context.previous(id))
    def apply[O2, R2](id: Id[O2]): O2 = ss(context(id))
    def send[I2 <:Id[O2], O2, R2](id: I2, msg: Msg[I2, O2, R2, C]): R2 = ss(context.send(id, msg))

    def ss[X](f:  => CVal[X, C]): X = { val s = self ; val result = f ; set(CVal(s, result.context)) ; result.value }
    def s[X](c: V[I], f:  => X): V[X] = {
      try { set(c) ; val result = f ; val cc = context ; CVal(result, cc) }
      finally { set(null) } // ALWAYS clean local thread var to prevent leakage }
    }
  }

  trait AMsg[I <: Id[O], O, +R, C <: Context[C]] extends CMsg[I, O, R, C] {
    def pre: Boolean
    def app: O
    def eff: R
    def pst: Boolean

    def preCondition2: Boolean = pre
    def apply2: O = app
    def effect2: R = eff
    def postCondition2: Boolean = pst
  }

  trait AId[+O] extends Id[O] {
    def ini: O
    def init: O = ini
  }

  case class SimpleContext(prev: SimpleContext = null, state: Map[Id[_], _] = Map()) extends Context[SimpleContext] {
    def previous[O](id: Id[O]): V[O] = CVal(prev(id).value, this)
    def apply[O](id: Id[O]): V[O] = CVal(state.getOrElse(id, id.init).asInstanceOf[O], this)
    def send[I <: Id[O], O, R](id: I, msg: Msg[I, O, R, SimpleContext]): V[R] = {
      if (!msg.preCondition(CVal(id, this)).value) throw sys.error("Pre failed")
      else {
        val eff = msg.effect(CVal(id, SimpleContext(this, state + (id -> msg.apply(CVal(id, this)).value))))
        if (msg.postCondition(CVal(id, SimpleContext(this, eff.context.state))).value) eff
        else throw sys.error("Post failed")
      }
    }
  }

  trait SMsg[I <: Id[O], O] extends AMsg[I, O, Unit, SimpleContext]

  case class AccountId(IBAN: String) extends AId[Account] {
    def ini = Account()
  }

  case class Account(balance: Double = 0.0)

  case class Open(init: Double) extends SMsg[AccountId, Account] {
    def pre = init > 0.0
    def app = Account(init)
    def eff = { }
    def pst = obj.balance == init
  }

  case class Withdraw(amount: Double) extends SMsg[AccountId, Account]  {
    def pre = amount > 0.0 && obj.balance >= amount
    def app = Account(obj.balance - amount)
    def eff = { }
    def pst = obj.balance == prev.balance - amount
  }

  case class Deposit(amount: Double) extends SMsg[AccountId, Account]  {
    def pre = amount > 0.0
    def app = Account(obj.balance + amount)
    def eff = { }
    def pst = obj.balance == prev.balance + amount
  }

  case class TransferId(id: Long) extends AId[Transfer] {
    def ini = Transfer()
  }

  case class Transfer(from: AccountId = null, to: AccountId = null, amount: Double = 0.0)

  case class Book(from: AccountId, to: AccountId, amount: Double) extends SMsg[TransferId, Transfer] {
    def pre = amount >= 0.0
    def app = Transfer(from, to, amount)
    def eff = { send(from, Withdraw(amount)) ; send(to, Deposit(amount)) }
    def pst = this(from).balance + this(to).balance == prev(from).balance + prev(to).balance
  }

  def main(args: Array[String]): Unit = {
    val a1 = AccountId("A1")
    val a2 = AccountId("A2")
    val t1 = TransferId(1)

    try {
      val c2 = SimpleContext().
        send(a1, Open(50)).
        send(a2, Open(80)).
        send(t1, Book(a1, a2, 30))

      println("c2: " + c2.context.state)
    }
    catch {
      case e: Exception => e.printStackTrace()
    }

    println("var: " + cVal.get())
  }
}

