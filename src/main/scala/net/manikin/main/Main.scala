package net.manikin.main

object Main {

  trait Id[+O] {
    def init: O

    def v_obj(implicit c: MutableContext): VObj[O] = c.read(this)
    def old_v_obj(implicit c: MutableContext): VObj[O] = c.read_old(this)

    def obj(implicit c: MutableContext): O = c.read(this).obj
    def old_obj(implicit c: MutableContext): O = c.read_old(this).obj

    def ![O2 >: O, R](msg: Msg[Id[O2], O2, R])(implicit c: MutableContext) = c.send(this, msg)
  }

  trait VObj[+O] {
    def obj: O
    def version: Long
    def fingerPrint: Int
  }

  case class DefaultVObj[+O](obj: O, version: Long, fingerPrint: Int) extends VObj[O]

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
    def branch: MutableContext
    def read[O](id: Id[O]): VObj[O]
    def read_old[O](id: Id[O]): VObj[O]
    def send[O, R](id: Id[O], message: Msg[Id[O], O, R]): R
  }

  class DefaultMutableContext(val init: World) extends MutableContext with Cloneable {
    var old: World = init
    var current: World = init

    private def copySelf: DefaultMutableContext = this.clone().asInstanceOf[DefaultMutableContext]

    def branch: MutableContext = { val s = copySelf ; s.current = s.current.branch ; s }
    def read[O](id: Id[O]): VObj[O] = { val (obj, w2) = current.read(id) ; current = w2 ; obj }
    def read_old[O](id: Id[O]): VObj[O] = { val (obj, w2) = old.read(id) ; old = w2 ; obj }
    def send[O, R](id: Id[O], message: Msg[Id[O], O, R]): R = {
      val (result, w2) = current.send(id, message) ; current = w2 ; result
    }

    def withCurrent(w: World): Unit = { old = init ; current = w }
  }

  trait MutableSelfContext[+I <: Id[O], O] extends MutableContext {
    def self: I
  }

  class DefaultMutableSelfContext[+I <: Id[O], O](val self: I, init: World)
    extends DefaultMutableContext(init) with MutableSelfContext[I, O]

  type STATE = Map[Id[_], VObj[_]]

  trait World {
    def read[O](id: Id[O]): (VObj[O], World)
    def send[O, R](id: Id[O], message: Msg[Id[O], O, R]): (R, World)

    def branch: World
    def merge(other: World): World

    def actions: Vector[ACTION[_]]
    def latest(state: STATE): STATE
  }

  def vobj[O](id: Id[O]): VObj[O] = {
    val obj = id.init
    DefaultVObj(obj, 0, obj.hashCode())
  }

  import scala.util.hashing.MurmurHash3._

  case class DefaultWorld(parent: World = null, state: STATE = Map(), actions: Vector[ACTION[_]] = Vector()) extends World {
    def branch: World = DefaultWorld(this, Map(), Vector())
    def get[O](id: Id[O]): VObj[O] = state.getOrElse(id, vobj(id)).asInstanceOf[VObj[O]]
    def read[O](id: Id[O]): (VObj[O], World) = read2(id)
    def read2[O](id: Id[O]): (VObj[O], DefaultWorld) = {
      val v_obj = {
        if (!state.contains(id) && parent != null) parent.latest(Map(id -> get(id)))(id).asInstanceOf[VObj[O]]
        else get(id)
      }
      val read = DefaultRead[Id[O], O](id, v_obj.version, v_obj.fingerPrint)
      (v_obj, this.copy(state = state + (id -> v_obj), actions = actions :+ read))
    }
    def send[O, R](id: Id[O], message: Msg[Id[O], O, R]): (R, World) = {
      val (old_v_obj, latest) = read2(id)
      val read_only_world = ReadOnlyWrapper(latest.branch)

      val c = new DefaultMutableSelfContext[Id[O], O](id, read_only_world)
      message._ctx = c

      if (!message.pre) throw new RuntimeException("Pre failed")
      else {
        val preReadActions = (c.old.actions ++ c.current.actions).map(_.asInstanceOf[READ[_]]) // only reads allowed

        c.withCurrent(read_only_world)
        val new_obj = message.app
        val appReadActions = (c.old.actions ++ c.current.actions).map(_.asInstanceOf[READ[_]]) // only reads allowed
        if (!appReadActions.forall(_.id == id)) throw new RuntimeException("app may only read itself")

        val new_state = state + (id -> DefaultVObj(new_obj, old_v_obj.version + 1, mix(old_v_obj.fingerPrint, message.hashCode())))
        c.withCurrent(DefaultWorld(this, new_state, Vector()))
        val result = message.eff
        val effActions = c.old.actions.map(_.asInstanceOf[READ[_]]) ++ c.current.actions // only read actions in old

        c.withCurrent(ReadOnlyWrapper(c.current))

        if (!message.pst) throw new RuntimeException("Post failed")
        else {
          val pstActions = (c.old.actions ++ c.current.actions).map(_.asInstanceOf[READ[_]]) // only reads allowed
          val send = DefaultSend(id, old_v_obj.version, old_v_obj.fingerPrint, message, preReadActions, effActions, result, pstActions)
          val sub_state = c.current.latest(send.minWrites.map(_.id).map(id => (id, get(id))).toMap)

          (result, latest.copy(state = new_state ++ sub_state, actions = actions :+ send))
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

      if ((minReads ++ minWrites).exists{p =>
        val v_obj = get(p.id)
        v_obj.version != p.version || v_obj.fingerPrint != p.fingerPrint
      }) throw new RuntimeException("CANNOT MERGE")

      val sub_state = other.latest(minWrites.map(_.id).map(id => (id, get(id))).toMap)

      DefaultWorld(this, state ++ sub_state, actions ++ postfix_actions)
    }

    def latest(other: STATE): STATE = {
      other.keys.map { id =>
        val v_obj = {
          if (!state.contains(id) && parent != null) parent.latest(Map(id -> get(id)))(id)
          else get(id)
        }
        (id, v_obj)
      }.toMap
    }
  }

  case class ReadOnlyWrapper(other: World, actions: Vector[READ[_]]= Vector()) extends World {
    def branch: World = throw new RuntimeException("Cannot branch Read Only World")
    def read[O](id: Id[O]): (VObj[O], World) = {
      val (v_obj, n_other) = other.read(id)
      (v_obj, ReadOnlyWrapper(n_other, actions :+ DefaultRead[Id[O], O](id, v_obj.version, v_obj.fingerPrint)))
    }
    def send[O, R](id: Id[O], message: Msg[Id[O], O, R]): (R, World) = throw new RuntimeException("Cannot send to Read Only World")
    def merge(other: World): World = throw new RuntimeException("Cannot merge to Read Only World")
    def latest(state: STATE): STATE = other.latest(state)
  }

  case class VId[+O](id: Id[O], version: Long, fingerPrint: Int)

  trait Action[+I <: Id[O], O] {
    def id: I
    def version: Long
    def fingerPrint: Int

    def minReads: Set[VId[_]]
    def minWrites: Set[VId[_]]
  }

  trait Read[+I <: Id[O], O] extends Action[I, O] {
    def minWrites = Set()
  }

  def minVIds(seq: Seq[VId[_]]): Set[VId[_]] = seq.groupBy(_.id).map(_._2.minBy(_.version)).toSet

  trait Send[+I <: Id[O], O, +R] extends Action[I, O] {
    def message: Msg[I, O, R]
    def preActions: Seq[READ[_]]
    def effActions: Seq[ACTION[_]]
    def pstActions: Seq[READ[_]]
  }

  case class DefaultRead[+I <: Id[O], O](id: I, version: Long, fingerPrint: Int) extends Read[I, O] {
    def minReads = Set(VId(id, version, fingerPrint))
  }

  case class DefaultSend[+I <: Id[O], O, +R](
    id: I,
    version: Long,
    fingerPrint: Int,
    message: Msg[I, O, R],
    preActions: Seq[READ[_]],
    effActions: Seq[ACTION[_]],
    effResult: R,
    pstActions: Seq[READ[_]]
  ) extends Send[I, O, R] {
    def minReads = minVIds(VId(id, version, fingerPrint) +: (preActions ++ effActions ++ pstActions).flatMap(_.minReads))
    def minWrites = minVIds(VId(id, version, fingerPrint) +: effActions.flatMap(_.minWrites))
  }

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
    implicit var w = new DefaultMutableContext(DefaultWorld())

    val a1 = AccountId("A1")
    val a2 = AccountId("A2")
    val t1 = TransferId(1)

    val a3 = AccountId("A3")
    val a4 = AccountId("A4")
    val t2 = TransferId(2)

    a1 ! OpenAccount(100)
    a2 ! OpenAccount(50)

    val w0 = w.current

    w = new DefaultMutableContext(DefaultWorld())

    a3 ! OpenAccount(40)
    a4 ! OpenAccount(60)
    a4 ! Withdraw(30)
    val w2 = w.current
    
    println("w0: " + w0.actions)
    println("w2: " + w2.actions)
    
    w.current = w0.merge(w2)

    println("w3: " + a1.obj)
    w.current
  }
}