package net.manikin.core

object TransactionalObject {

  import scala.language.implicitConversions

  trait Id[+X] {
    def init: X

    def prev(implicit ctx: Context): X = ctx.previous(this)
    def apply()(implicit ctx: Context): X = ctx(this)
  }

  implicit class IdSyntax[X](id: Id[X]) {
    def <~[I <: Id[X], R](msg: Message[X, I, R])(implicit ctx: Context): R = ctx.send(id.asInstanceOf[I], msg)
    def update(x: X)(implicit ctx: Context): Unit = ctx.set(id, x)
  }
  
  trait Message[+X, I <: Id[X], +R] {

    // context and self will be injected
    var contextVar: Context = _
    var selfVar: I = _

    implicit def context: Context = contextVar
    def self: I = selfVar

    def pre: Boolean
    def app: R
    def pst: Boolean
  }
  
  trait Context {
    def apply[X](id: Id[X]): X
    def set[X](id: Id[X], x: X): Unit
    def send[X, I <: Id[X], R](id: I, message: Message[X, I, R]): R

    def withFailure(f: Failure): Unit
    def previous: Context
  }
  
  trait Failure

  case class FailureException(f: Failure) extends Exception {
    override def toString = "FailureException(" + f + ")"
  }
  case class ExceptionFailure(t: Throwable) extends Failure {
    override def toString = "ExceptionFailure(" + t + ")\n" + t.getStackTrace.toList.mkString("\n")
  }
}