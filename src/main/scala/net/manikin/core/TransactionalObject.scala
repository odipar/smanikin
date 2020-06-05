package net.manikin.core

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

  trait Context {
    def apply[X](id: Id[X]): X
    def set[X](id: Id[X], x: X): Unit
    def send[X, R](id: Id[X], message: Message[X, R]): R

    def withFailure(f: Failure): Unit
    def previous: Context
  }

  case class NextStateException[+X, +R](id: Id[X], state: X, message: Message[X, R]) extends Failure

  case class VId[+X](id: Id[X], version: Long) {
    override def toString: String = version + ":" + id
  }

  case class Send[+X, +R](level: Int, vid: VId[X], message: Message[X, R])

  trait Id[+X] {
    def init: X

    def prev(implicit ctx: Context): X = ctx.previous(this)
    def apply()(implicit ctx: Context): X = ctx(this)
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
    var contextVar: Context = _
    var selfVar: Id[_] = _

    implicit def context: Context = contextVar

    def self: ID = selfVar.asInstanceOf[ID]

    def pre: Boolean
    def app: R
    def pst: Boolean
  }

  implicit class MessageSyntax[X, +R](t: Message[X, R]) {
    def -->(id: Id[X])(implicit ctx: Context): R = ctx.send(id, t)
  }

  implicit class IdSyntax[X](id: Id[X]) {
    def <--[R](t: Message[X, R])(implicit ctx: Context): R = ctx.send(id, t)
    def update(x: X)(implicit ctx: Context): Unit = ctx.set(id, x)
  }

  implicit class StateIdSyntax[+X](id: DataID[X]) {
    def apply()(implicit ctx: Context): X = id.self().data
    def prev(implicit ctx: Context): X = id.self.prev.data
  }

  implicit class StateIdSyntax2[X](id: DataID[X]) {
    def update(x: X)(implicit ctx: Context): Unit = ctx.set(id.self, id.self().copy(data = x))
  }
}