package net.manikin.core

import net.manikin.core.TransObject.Id

object TransObject {
  import scala.language.implicitConversions
  
  // An Id that identifies an Object O
  trait Id[+O] {
    def init: O

    def version(implicit ctx: Context) : Long = ctx(this).version
    def old_version(implicit ctx: Context) : Long = ctx.previous(this).version

    def obj(implicit ctx: Context): O = ctx(this).obj
    def old_obj(implicit ctx: Context): O = ctx.previous(this).obj

    def typeString = this.init.getClass.getName.replace("$", ".")

    def ![I <: Id[O2], O2 >: O, R](msg: Message[I, O2, R])(implicit ctx: Context) = ctx.send(this, msg)
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

  trait Message[+I <: Id[O], O, +R] {
    // MessageContext will be injected
    @volatile private[core] var msgContext: MessageContext[_] = _

    implicit def context: Context = msgContext.context
    def self: I = msgContext.id.asInstanceOf[I]

    def pre: Boolean
    def app: O
    def eff: R
    def pst: Boolean

    def obj = self.obj
    def old_obj = self.old_obj
    
    def _retries_ : Int = context.retries
    def typeString : String = this.getClass.getName.replace("$", ".")
  }
  
  /*
   * A Context acts as a 'memory' to resolve Ids to Objects, and tracks all (versioned) Objects
   * To send a Message to an Object, there MUST always be an implicit Context in scope (Scala magic)
   * A Context implementation is optionally responsible for Transactional guarantees when a Message is committed
   */
  trait Context {
    def apply[O](id: Id[O]): VObject[O]
    def previous[O](id: Id[O]): VObject[O]
    def send[O, R](id: Id[O], message: Message[Id[O], O, R]): R
    
    def retries: Int
  }

  type ID = Id[_]
  type ST = Map[ID, VObject[_]]
  type MV = Map[ID, Long]
  
  case class MessageContext[+O](id: Id[O], context: Context)

  // Objects are versioned by Contexts, equality is based on object content, versions are ignored.
  final case class VObject[+O](version: Long, obj: O) {
    override def equals(other: Any) = other match {
      case v: VObject[O] => obj == v.obj
      case _ => false
    }
    override def hashCode = obj.hashCode
  }

  // Things can go wrong and that's encapsulated as type of Failure, not an Exception
  trait Failure

  case class FailureException(f: Failure) extends Exception {
    override def toString = "FailureException(" + f + ")"
  }
  
  case class ExceptionFailure(t: Throwable) extends Failure {
    override def toString = "ExceptionFailure(" + t + ")"
  }
}