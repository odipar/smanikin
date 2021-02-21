package net.manikin.main

object Main {

  trait Id[+O] {
    def init: O
  }

  trait Msg[I <: Id[O], +O, +R, W <: World[W]]{
    type V[+X] = WVal[X, W]

    def preCondition: V[I] => V[Boolean]
    def apply: V[I] => V[O]
    def effect: V[I] => V[R]
    def postCondition: V[I] => V[Boolean]
  }

  trait World[W <: World[W]] {
    type V[+X] = WVal[X, W]

    def apply[O](id: Id[O]): V[O]
    def previous[O](id: Id[O]): V[O]
    def send[I <: Id[O], O, R](id: I, msg: Msg[I, O, R, W]): V[R]
  }

  case class WVal[+R, W <: World[W]](value: R, world: W) extends World[W] {
    def apply[O](id: Id[O]): V[O] = world(id)
    def previous[O](id: Id[O]): V[O] = world.previous(id)
    def send[I <: Id[O], O, R2](id: I, msg: Msg[I, O, R2, W]): V[R2] = world.send(id, msg)
  }

  val cVal = new ThreadLocal[WVal[_, _]]

  trait CMsg[I <: Id[O], +O, +R, W <: World[W]] extends Msg[I, O, R, W] {
    def preCondition2: Boolean
    def apply2: O
    def effect2: R
    def postCondition2: Boolean

    def preCondition = c => s(c, preCondition2)
    def apply = c => s(c, apply2)
    def effect = c => s(c, effect2)
    def postCondition = c => s(c, postCondition2)

    def world: W = get().world
    def self: I = get().value
    def obj: O = ss(world(self))
    def prev: O = ss(world.previous(self))

    def get(): V[I] = cVal.get().asInstanceOf[V[I]]
    def set(c: V[I]): Unit = cVal.set(c)

    def prev[O2, R2](id: Id[O2]): O2 = ss(world.previous(id))
    def apply[O2, R2](id: Id[O2]): O2 = ss(world(id))
    def send[I2 <:Id[O2], O2, R2](id: I2, msg: Msg[I2, O2, R2, W]): R2 = ss(world.send(id, msg))

    def ss[X](f:  => WVal[X, W]): X = {
      val s = self ; val result = f ; set(WVal(s, result.world)) ; result.value
    }
    def s[X](c: V[I], f:  => X): V[X] = {
      try { set(c) ; val result = f ; val cc = world ; WVal(result, cc) }
      finally { set(null) } // ALWAYS clean local thread var to prevent leakage }
    }
  }

  trait AMsg[I <: Id[O], +O, +R, W <: World[W]] extends CMsg[I, O, R, W] {
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

  case class SimpleWorld(prev: SimpleWorld = null, state: Map[Id[_], _] = Map()) extends World[SimpleWorld] {
    def previous[O](id: Id[O]): V[O] = WVal(prev(id).value, this)
    def apply[O](id: Id[O]): V[O] = WVal(state.getOrElse(id, id.init).asInstanceOf[O], this)
    def send[I <: Id[O], O, R](id: I, msg: Msg[I, O, R, SimpleWorld]): V[R] = {
      if (!msg.preCondition(WVal(id, this)).value) throw sys.error("Pre failed")
      else {
        val eff = msg.effect(WVal(id, SimpleWorld(this, state + (id -> msg.apply(WVal(id, this)).value))))
        if (msg.postCondition(WVal(id, SimpleWorld(this, eff.world.state))).value) eff
        else throw sys.error("Post failed")
      }
    }
  }

  trait SMsg[I <: Id[O], +O, W <: World[W]] extends AMsg[I, O, Unit, SimpleWorld]

  case class AccountId(IBAN: String) extends AId[Account] {
    def ini = Account()
  }

  case class Account(balance: Double = 0.0)

  case class Open[W <: World[W]](init: Double) extends SMsg[AccountId, Account, W] {
    def pre = init > 0.0
    def app = Account(init)
    def eff = { }
    def pst = obj.balance == init
  }

  case class Withdraw[W <: World[W]](amount: Double) extends SMsg[AccountId, Account, W]  {
    def pre = amount > 0.0 && obj.balance >= amount
    def app = Account(obj.balance - amount)
    def eff = { }
    def pst = obj.balance == prev.balance - amount
  }

  case class Deposit[W <: World[W]](amount: Double) extends SMsg[AccountId, Account, W]  {
    def pre = amount > 0.0
    def app = Account(obj.balance + amount)
    def eff = { }
    def pst = obj.balance == prev.balance + amount
  }

  case class TransferId(id: Long) extends AId[Transfer] {
    def ini = Transfer()
  }

  case class Transfer(from: AccountId = null, to: AccountId = null, amount: Double = 0.0)

  case class Book[W <: World[W]](from: AccountId, to: AccountId, amount: Double) extends SMsg[TransferId, Transfer, W] {
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
      val c2 = SimpleWorld().
        send(a1, Open(50)).
        send(a2, Open(80)).
        send(t1, Book(a1, a2, 30))

      println("c2: " + c2.world.state)
    }
    catch {
      case e: Exception => e.printStackTrace()
    }

    println("var: " + cVal.get())
  }
}

