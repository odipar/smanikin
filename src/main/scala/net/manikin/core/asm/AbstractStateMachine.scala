package net.manikin.core.asm

object AbstractStateMachine {
  import scala.language.implicitConversions

  trait Failure

  case class FailureException(f: Failure) extends Exception {
    override def toString = "FailureException(" + f + ")"
  }

  case class PreFailed[+X](id: VId[X], state: X, transition: Transition[X]) extends Failure
  case class PostFailed[+X](id: VId[X], state: X, transition: Transition[X]) extends Failure
  case class ExceptionFailure(t: Throwable) extends Failure {
    override def toString = "ExceptionFailure(" + t + ")\n" + t.getStackTrace.toList.mkString("\n")
  }

  case class VId[+X](id: Id[X], version: Long) {
    override def toString: String = version + ":" + id
  }
  
  class Context() extends Cloneable {
    private var failure: Failure = null
    private var level = 0
    private var previousContext: Context = _
    private var vStateMap: Map[Id[_], Long] = Map()
    private var stateMap: Map[Id[_], _] = Map()
    private var reads: Set[VId[_]]= Set()
    private var writes: Set[VId[_]] = Set()
    private var sends: Vector[Send[_]] = Vector()

    def previous: Context = previousContext
    def sent: Vector[Send[_]] = sends
    def written: Set[VId[_]] = writes
    def read: Set[VId[_]] = reads
    def state: Map[Id[_], _] = stateMap
    def versions: Map[Id[_], Long] = vStateMap

    def version[X](id: Id[X]): Long = vStateMap.getOrElse(id, 0)

    def apply[X](id: Id[X]): X = {
      if (!vStateMap.contains(id)) { stateMap = stateMap + (id -> id.init) ; vStateMap = vStateMap + (id -> 0) }

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

    def revert(c: Context): Unit = {
      previousContext = c.previousContext
      vStateMap = c.vStateMap
      reads = c.reads
      writes = c.writes
      sends = c.sends
    }

    def send[X](id: Id[X], transition: Transition[X]): Unit = {
      val vid = VId(id, version(id))
      val previous_context: Context = this.clone.asInstanceOf[Context]


      transition.selfVar = id
      transition.contextVar = this

      level += 1
      sends = Vector()

      try {
        if (transition.pre) {
          transition.app         
          previousContext = previous_context
          if (transition.pst) {
            level -= 1
            sends = (previous_context.sends :+ Send(level, vid, transition)) ++ sends
          }
          else {
            failure = PostFailed(vid, id()(this), transition)
            throw new FailureException(failure)
          }
        }
        else {
          failure = PreFailed(vid, id()(this), transition)
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

  case class Send[+X](level: Int, vid: VId[X], transition: Transition[X])

  trait Id[+X] {
    def init: X

    def previous(implicit ctx: Context): X = ctx.previous(this)
    def apply()(implicit ctx: Context): X = ctx(this)
  }

  case class State[+X](data: X, state: String)
  
  trait StateID[+X] extends Id[State[X]] {
    def init = State(initData, "Initial")
    def initData: X
  }
  
  case class DataID[+X](self: Id[State[X]])

  trait StateTransition[+X] extends Transition[State[X]] {
    def nst: PartialFunction[String, String]
    def data = DataID(self)

    def app: Unit = { self() = self().copy(state = nst(self().state)) ; ap2 }
    def ap2: Unit
  }
  
  trait Transition[+X] {
    var contextVar: Context = _
    var selfVar: Id[_] = _

    implicit def context: Context = contextVar

    def self: Id[X] = selfVar.asInstanceOf[Id[X]]

    def pre: Boolean
    def app: Unit
    def pst: Boolean
  }

  implicit class TransitionSyntax[X](t: Transition[X]) {
    def -->(id: Id[X])(implicit ctx: Context): Unit = ctx.send(id, t)
  }

  implicit class IdContext[X](id: Id[X]) {
    def update(x: X)(implicit ctx: Context): Unit = ctx.set(id, x)
  }

  implicit class StateIdContext[X](id: DataID[X]) {
    def apply()(implicit ctx: Context): X = id.self().data
    def previous(implicit ctx: Context): X = id.self.previous.data
    def update(x: X)(implicit ctx: Context): Unit = ctx.set(id.self, id.self().copy(data = x))
  }
}