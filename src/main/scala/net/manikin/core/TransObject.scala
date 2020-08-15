package net.manikin.core

import net.manikin.core.TransObject.Id

object TransObject {
  import scala.language.implicitConversions
  
  // An Id that identifies an Object O
  trait Id[+O] {
    def init: O

    def failures(implicit ctx: World): Int = ctx.failures(this)
    def version(implicit ctx: World): Long = ctx(this).version
    def old_version(implicit ctx: World): Long = ctx.previous(this).version

    def obj(implicit ctx: World): O = ctx(this).obj
    def old_obj(implicit ctx: World): O = ctx.previous(this).obj

    def typeString = this.init.getClass.getName.replace("$", ".")

    def ![O2 >: O, R](msg: Message[Id[O2], O2, R])(implicit ctx: World) = ctx.send(this, msg)
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

    implicit def context: World = msgContext.world
    def self: I = msgContext.id.asInstanceOf[I]

    def pre: Boolean
    def app: O
    def eff: R
    def pst: Boolean

    def obj = self.obj
    def old_obj = self.old_obj

    def failures: Int = self.failures

    def typeString : String = this.getClass.getName.replace("$", ".")
  }

  /* Generic snapshot of the full Object state */
  case class Snapshot[+I <: Id[O], O](o: O) extends Message[I, O, Unit] {
    def pre = o != null
    def app = o
    def eff = { }
    def pst = self.obj == o
  }

  /*
   * A World acts as a 'memory' to resolve Ids to Objects, and tracks all (versioned) Objects
   * To send a Message to an Object, there MUST always be an implicit Context in scope (Scala magic)
   * A World implementation is optionally responsible for Transactional guarantees when Messages are committed
   */
  trait World {
    def apply[O](id: Id[O]): VObject[O]
    def previous[O](id: Id[O]): VObject[O]
    def send[O, R](id: Id[O], message: Message[Id[O], O, R]): R

    def failures[O](id: Id[O]): Int
  }

  // Some convenient type defs
  type ID = Id[_]
  type ST = Map[ID, VObject[_]]
  type MV = Map[ID, Long]

  // A product of an Id and a World
  final case class MessageContext[+O](id: Id[O], world: World)

  // Objects are versioned by Worlds. VObject equality is based solely on object content, with versions ignored.
  final case class VObject[+O](version: Long, serial_id: Long, obj: O) {
    override def equals(other: Any) = other match {
      case v: VObject[O] => obj == v.obj
      case _ => false
    }
    def withSerial(s_id: Long) = VObject(version, s_id, obj)
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