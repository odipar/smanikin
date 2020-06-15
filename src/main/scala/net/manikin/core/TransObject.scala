package net.manikin.core

object TransObject {
  import java.util.UUID
  import scala.language.implicitConversions

  // An Id that identifies an Object O
  trait Id[+O] {
    def init: O

    def obj(implicit ctx: Context): O = ctx(this).obj
    def old_obj(implicit ctx: Context): O = ctx.previous(this).obj
  }

  /*
   * A Message can be sent to an Object O with address Id, returning R
   *
   * There are four stages:
   *
   * 1) (pre)pre-condition: predicate over anything. MUST BE a pure function (no side effects)
   * 2) (app)application: returns a new version of an Object. MUST BE a pure function (used for Event sourcing)
   * 3) (eff)effects: can do anything, but should return R
   * 4) (pst)post-condition: predicate over anything, including previous states. MUST BE a pure function
  */

  trait Message[+O, +I <: Id[O], +R] {
    // context and this will be injected
    var contextVar: Context = null
    var thisVar: Id[_] = null // vars cannot be covariant, so hack it

    implicit def context: Context = contextVar
    def self: I = thisVar.asInstanceOf[I]

    def pre: Boolean
    def app: O
    def eff: R
    def pst: Boolean
  }

  /*
   * A Context acts as a 'memory' to resolve Ids to Objects, and tracks all (versioned) Objects
   * To send a Message to an Object, there MUST always be an implicit Context in scope (Scala magic)
   * A Context implementation is optionally responsible for Transactional guarantees when a Message is committed
   */
  trait Context {
    def apply[O](id: Id[O]): VObject[O]
    def send[O, I <: Id[O], R](id: I, message: Message[O, I, R]): R

    def failure: Failure
    def previous: Context
  }

  // Objects are versioned by Contexts
  case class VObject[+O](version: Long, obj: O)

  // Things can go wrong and that's encapsulated as type of Failure, not an Exception
  trait Failure

  case class FailureException(f: Failure) extends Exception {
    override def toString = "FailureException(" + f + ")"
  }
  
  case class ExceptionFailure(t: Throwable) extends Failure {
    override def toString = "ExceptionFailure(" + t + ")\n" + t.getStackTrace.toList.mkString("\n")
  }

  implicit class IdSyntax[O](id: Id[O]) {
    def ![I <: Id[O], R](msg: Message[O, I, R])(implicit ctx: Context): R = ctx.send(id, msg)
  }
  
  case class TId(uuid: UUID = UUID.randomUUID()) extends Id[Unit] { def init: Unit = { } }

  trait Transaction[+R] extends Message[Unit, TId, R] {
    def pre = true
    def app = { }
    def pst = true
  }
}