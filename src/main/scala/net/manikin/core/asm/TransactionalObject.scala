package net.manikin.core.asm

object TransactionalObject {

  import scala.language.implicitConversions

  trait Failure

  case class FailureException(f: Failure) extends Exception {
    override def toString = "FailureException(" + f + ")"
  }

  case class PreFailed[+X, +R](id: VId[X], state: X, message: Message[X, R]) extends Failure
  case class PostFailed[+X, +R](id: VId[X], state: X, message: Message[X, R]) extends Failure
  case class ExceptionFailure(t: Throwable) extends Failure {
    override def toString = "ExceptionFailure(" + t + ")\n" + t.getStackTrace.toList.mkString("\n")
  }

  class TransactionContext() extends Cloneable {
    private var failure: Failure = null
    private var level = 0
    private var previousContext: TransactionContext = _
    private var vStateMap: Map[Id[_], Long] = Map()
    private var stateMap: Map[Id[_], _] = Map()
    private var reads: Set[VId[_]] = Set()
    private var writes: Set[VId[_]] = Set()
    private var sends: Vector[Send[_, _]] = Vector()

    def previous: TransactionContext = previousContext
    def sent: Vector[Send[_, _]] = sends
    def written: Set[VId[_]] = writes
    def read: Set[VId[_]] = reads
    def allState: Map[Id[_], _] = stateMap
    def versions: Map[Id[_], Long] = vStateMap

    def withFailure(f: Failure): Unit = failure = f
    def version[X](id: Id[X]): Long = vStateMap.getOrElse(id, 0)

    def apply[X](id: Id[X]): X = {
      if (!vStateMap.contains(id)) {stateMap = stateMap + (id -> id.init); vStateMap = vStateMap + (id -> 0)}

      reads = reads + VId(id, version(id))
      stateMap(id).asInstanceOf[X]
    }

    def failed: Failure = failure

    def set[X](id: Id[X], x: X): Unit = {
      val v = version(id)

      writes = writes + VId(id, v)
      stateMap = stateMap + (id -> x)
      vStateMap = vStateMap + (id -> (v + 1))
    }

    def revert(c: TransactionContext): Unit = {
      previousContext = c.previousContext
      vStateMap = c.vStateMap
      reads = c.reads
      writes = c.writes
      sends = c.sends
    }

    def send[X, R](id: Id[X], message: Message[X, R]): R = {
      val vid = VId(id, version(id))
      val previous_context: TransactionContext = this.clone.asInstanceOf[TransactionContext]

      message.selfVar = id
      message.contextVar = this

      level += 1
      sends = Vector()

      try {
        if (message.pre) {
          val result = message.app
          previousContext = previous_context
          if (message.pst) {
            level -= 1
            sends = (previous_context.sends :+ Send(level, vid, message)) ++ sends
            result
          }
          else {
            failure = PostFailed(vid, id()(this), message)
            throw new FailureException(failure)
          }
        }
        else {
          failure = PreFailed(vid, id()(this), message)
          throw new FailureException(failure)
        }
      }
      catch {
        case e: Throwable => {
          if (failure == null) failure = ExceptionFailure(e)
          revert(previous_context)
          throw e
        }
      }
    }
  }

  case class NextStateException[+X, +R](id: Id[X], state: X, message: Message[X, R]) extends Failure

  case class VId[+X](id: Id[X], version: Long) {
    override def toString: String = version + ":" + id
  }

  case class Send[+X, +R](level: Int, vid: VId[X], message: Message[X, R])

  trait Id[+X] {
    def init: X

    def prev(implicit ctx: TransactionContext): X = ctx.previous(this)
    def apply()(implicit ctx: TransactionContext): X = ctx(this)
  }

  case class State[+X](data: X, state: String)

  trait StateId[+X] extends Id[State[X]] {
    def init = State(initData, "Initial")
    def initData: X
  }

  case class DataID[+X](self: StateId[X])

  // State transition Message
  trait STMessage[+X, +R] extends Message[State[X], R] {
    type ID <: StateId[X]

    def data = DataID(self)

    def nst: PartialFunction[String, String]
    def app: R = {
      val st = self().state

      if (nst.isDefinedAt(st)) { self() = self().copy(state = nst(self().state)) ; apl }
      else {
        val f = NextStateException(self, self(), this)
        context.withFailure(f)
        throw FailureException(f)
      }
    }
    def apl: R
  }

  trait Message[+X, +R] {
    type ID <: Id[X]

    // context and self will be injected
    var contextVar: TransactionContext = _
    var selfVar: Id[_] = _

    implicit def context: TransactionContext = contextVar

    def self: ID = selfVar.asInstanceOf[ID]

    def pre: Boolean
    def app: R
    def pst: Boolean
  }

  implicit class MessageSyntax[X, +R](t: Message[X, R]) {
    def -->(id: Id[X])(implicit ctx: TransactionContext): R = ctx.send(id, t)
  }

  implicit class IdSyntax[X](id: Id[X]) {
    def <--[R](t: Message[X, R])(implicit ctx: TransactionContext): R = ctx.send(id, t)
    def update(x: X)(implicit ctx: TransactionContext): Unit = ctx.set(id, x)
  }

  implicit class StateIdSyntax[+X](id: DataID[X]) {
    def apply()(implicit ctx: TransactionContext): X = id.self().data
    def prev(implicit ctx: TransactionContext): X = id.self.prev.data
  }

  implicit class StateIdSyntax2[X](id: DataID[X]) {
    def update(x: X)(implicit ctx: TransactionContext): Unit = ctx.set(id.self, id.self().copy(data = x))
  }
}