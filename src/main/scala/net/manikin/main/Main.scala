package net.manikin.main

import net.manikin.core.TransObject.{Id, Message}

object Main {

  trait Id[+O] {
    def init: O

    def obj(implicit c: MutableContext): O = c.read(this).obj
    def old_obj(implicit c: MutableContext): O = c.read_old(this).obj

    def ![O2 >: O, R](msg: Msg[Id[O2], O2, R])(implicit c: MutableContext) = c.send(this, msg)
  }

  trait VObj[+O] {
    def obj: O
    def version: Long
  }

  case class DefaultVObj[+O](obj: O, version: Long) extends VObj[O]

  trait Msg[+I <: Id[O], O, +R] {
    var _ctx: MutableSelfContext[_ <: Id[O], O] = _
    implicit def ctx: MutableSelfContext[I, O] = _ctx.asInstanceOf[MutableSelfContext[I, O]]

    def self: I = ctx.self
    def obj: O = self.obj
    def old_obj: O = self.old_obj

    def pre: Boolean
    def app: O
    def eff: R
    def pst: Boolean
  }

  trait Context

  trait MutableContext extends Context {
    def read[O](id: Id[O]): VObj[O]
    def read_old[O](id: Id[O]): VObj[O]
    def send[O, R](id: Id[O], message: Msg[Id[O], O, R]): R
  }

  class DefaultMutableContext(var old: World, var current: World) extends MutableContext {
    def read[O](id: Id[O]): VObj[O] = { val (obj, w2) = current.read(id) ; current = w2 ; obj }
    def read_old[O](id: Id[O]): VObj[O] = { val (obj, w2) = old.read(id) ; old = w2 ; obj }
    def send[O, R](id: Id[O], message: Msg[Id[O], O, R]): R = {
      val (result, w2) = current.send(id, message)
      current = w2
      result
    }
  }

  trait MutableSelfContext[+I <: Id[O], O] extends MutableContext {
    def self: I
  }

  class DefaultMutableSelfContext[+I <: Id[O], O](val self: I, old: World, current: World)
    extends DefaultMutableContext(old, current) with MutableSelfContext[I, O]

  type STATE = Map[Id[_], VObj[_]]

  trait World {
    def read[O](id: Id[O]): (VObj[O], World)
    def send[O, R](id: Id[O], message: Msg[Id[O], O, R]): (R, World)
    def merge(other: World): World

    def actions: Vector[ACTION[_]]
    def latest(state: STATE): STATE
  }

  case class DefaultWorld(state: STATE = Map(), actions: Vector[ACTION[_]] = Vector()) extends World {
    def get[O](id: Id[O]): VObj[O] = state.getOrElse(id, DefaultVObj(id.init, 0)).asInstanceOf[VObj[O]]
    def read[O](id: Id[O]): (VObj[O], World) = {
      val v_obj = get(id)
      val read = DefaultRead[Id[O], O](id, v_obj.version)
      (v_obj, this.copy(actions = actions :+ read))
    }
    def send[O, R](id: Id[O], message: Msg[Id[O], O, R]): (R, World) = {
      val old_v_obj = get(id)
      val read_only_world = ReadOnlyWrapper(DefaultWorld(state, Vector()))

      val c = new DefaultMutableSelfContext[Id[O], O](id, read_only_world, read_only_world)
      message._ctx = c

      if (!message.pre) throw new RuntimeException("Pre failed")
      else {
        val preReadActions = (c.old.actions ++ c.current.actions).map(_.asInstanceOf[READ[_]]) // only reads allowed

        c.old = read_only_world
        c.current = read_only_world
        
        val new_obj = message.app

        val appReadActions = (c.old.actions ++ c.current.actions).map(_.asInstanceOf[READ[_]]) // only reads allowed
        if (!appReadActions.forall(_.id == id)) throw new RuntimeException("app may only read itself")

        val new_state = state + (id -> DefaultVObj(new_obj, old_v_obj.version + 1))

        c.old = read_only_world
        c.current = DefaultWorld(new_state, Vector())

        val result = message.eff
        val effActions = c.old.actions.map(_.asInstanceOf[READ[_]]) ++ c.current.actions // only read actions in old

        c.old = read_only_world
        c.current = ReadOnlyWrapper(c.current)

        if (!message.pst) throw new RuntimeException("Post failed")
        else {
          val pstActions = (c.old.actions ++ c.current.actions).map(_.asInstanceOf[READ[_]]) // only reads allowed
          val send = DefaultSend(id, old_v_obj.version, message, preReadActions, effActions, result, pstActions)
          val sub_state = c.current.latest(send.minWrites.map(_.id).map(id => (id, get(id))).toMap)

          (result, this.copy(state = new_state ++ sub_state, actions = actions :+ send))
        }
      }
    }

    def merge(other: World): World = {
      val size = actions.size
      val other_actions = other.actions
      val other_size = other_actions.size

      var i = 0

      // common prefix (ancestor actions)
      while (i < other_size && i < size && actions(i) == other_actions(i)) { i += 1 }

      // non common postfix (divergent actions)
      val postfix_actions = other_actions.splitAt(i)._2

      val minReads = minVIds(postfix_actions.flatMap(_.minReads))
      val minWrites = minVIds(postfix_actions.flatMap(_.minWrites))

      if ((minReads ++ minWrites).exists(p => get(p.id).version != p.version)) throw new RuntimeException("CANNOT MERGE")

      val sub_state = other.latest(minWrites.map(_.id).map(id => (id, get(id))).toMap)

      DefaultWorld(state ++ sub_state, actions ++ postfix_actions)
    }

    def latest(state: STATE): STATE = state.keys.map(id => (id, get(id))).toMap
  }

  case class ReadOnlyWrapper(other: World, actions: Vector[READ[_]]= Vector()) extends World {
    def read[O](id: Id[O]): (VObj[O], World) = {
      val (v_obj, n_other) = other.read(id)
      (v_obj, ReadOnlyWrapper(n_other, actions :+ DefaultRead[Id[O], O](id, v_obj.version)))
    }
    def send[O, R](id: Id[O], message: Msg[Id[O], O, R]): (R, World) = throw new RuntimeException("Cannot send to Read Only World")
    def merge(other: World): World = throw new RuntimeException("Cannot merge to Read Only World")
    def latest(state: STATE): STATE = other.latest(state)
  }

  case class VId[+O](id: Id[O], version: Long)

  trait Action[+I <: Id[O], O] {
    def id: I
    def version: Long

    def minReads: Set[VId[_]]
    def minWrites: Set[VId[_]]
  }

  trait Read[+I <: Id[O], O] extends Action[I, O] {
    def minReads = Set(VId(id, version))
    def minWrites = Set()
  }

  def minVIds(seq: Seq[VId[_]]): Set[VId[_]] = seq.groupBy(_.id).map(_._2.minBy(_.version)).toSet

  trait Send[+I <: Id[O], O, +R] extends Action[I, O] {
    def message: Msg[I, O, R]
    def preActions: Seq[READ[_]]
    def effActions: Seq[ACTION[_]]
    def pstActions: Seq[READ[_]]

    def minReads = minVIds(VId(id, version) +: (preActions ++ effActions ++ pstActions).flatMap(_.minReads))
    def minWrites = minVIds(VId(id, version) +: effActions.flatMap(_.minWrites))
  }

  case class DefaultRead[+I <: Id[O], O](id: I, version: Long) extends Read[I, O]

  case class DefaultSend[+I <: Id[O], O, +R](
    id: I,
    version: Long,
    message: Msg[I, O, R],
    preActions: Seq[READ[_]],
    effActions: Seq[ACTION[_]],
    effResult: R,
    pstActions: Seq[READ[_]]
  ) extends Send[I, O, R]

  type READ[O] = Read[_ <: Id[O], O]
  type SEND[O] = Send[_ <: Id[O], O, _]
  type ACTION[O] = Action[_ <: Id[O], O]

  case class AccountId(iban: String) extends Id[Account] {
    def init = Account()
  }
  case class TransferId(id: Long) extends Id[Transfer] {
    def init = Transfer()
  }

  case class Account(balance: Long = 0)
  case class Transfer(from: AccountId = null, to: AccountId = null, amount: Long = 0)

  trait AccountMsg extends Msg[AccountId, Account, Unit]
  trait TransferMsg extends Msg[TransferId, Transfer, Unit]

  case class OpenAccount(initial: Long) extends AccountMsg {
    def pre = initial > 0
    def app = obj.copy(balance = initial)
    def eff = { }
    def pst = obj.balance == initial
  }

  case class Withdraw(amount: Long) extends AccountMsg {
    def pre = obj.balance >= amount && amount > 0
    def app = obj.copy(balance = obj.balance - amount)
    def eff = { }
    def pst = obj.balance == old_obj.balance - amount
  }

  case class Deposit(amount: Long) extends AccountMsg {
    def pre = amount > 0
    def app = obj.copy(balance = obj.balance + amount)
    def eff = { }
    def pst = obj.balance == old_obj.balance + amount
  }

  case class Book(from: AccountId, to: AccountId, amount: Long) extends TransferMsg {
    def pre = from != to && amount > 0
    def app = obj.copy(from = from, to = to, amount = amount)
    def eff = { from ! Withdraw(amount) ; to ! Deposit(amount) }
    def pst = from.old_obj.balance + to.old_obj.balance == from.obj.balance + to.obj.balance
  }

  case class Factorial(f: BigInt) extends Id[BigInt] {
    def init = 1
  }

  case class Calculate(c: BigInt = 1) extends Msg[Factorial, BigInt, Unit] {
    def pre = true
    def app = obj * c
    def eff = if (c < self.f) self ! Calculate(c + 1)
    def pst = obj == old_obj * (c to self.f).product
  }

  def main(args: Array[String]): Unit = {
    implicit var w = new DefaultMutableContext(DefaultWorld(), DefaultWorld())

    val a1 = AccountId("A1")
    val a2 = AccountId("A2")
    val t1 = TransferId(1)

    val a3 = AccountId("A3")
    val a4 = AccountId("A4")
    val t2 = TransferId(2)

    a1 ! OpenAccount(100)
    a2 ! OpenAccount(50)
    t1 ! Book(a1, a2, 30)

    val w1 = w.current

    a3 ! OpenAccount(80)
    a4 ! OpenAccount(40)
    t2 ! Book(a3, a4, 30)

    val w2 = w.current

    val w3 = w1.merge(w2)

    println("a1: " + w3.read(a1)._1)
    println("a2: " + w3.read(a2)._1)
    println("a3: " + w3.read(a3)._1)
    println("a4: " + w3.read(a4)._1)
  }
}