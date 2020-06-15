package net.manikin.core

import net.manikin.core.InMemoryStore.InMemoryStore

object DefaultContext {
  import TransObject._

  // A DefaultContext keeps track of (historical) Object states and Message dispatches
  case class DefaultContext(private val store: Store = new InMemoryStore()) extends Context with Cloneable {
    type ID = Id[_]
    type MV = Map[ID, Long]
    type ST = Map[ID, VObject[_]]
    
    private var level = 0
    private var failure_ : Failure = _
    private var previous_ : DefaultContext = _
    var state: ST = Map()
    var reads: MV = Map()
    var writes: MV = Map()
    var sends: Vector[STYPE] = Vector()

    def copyThis(): DefaultContext = this.clone().asInstanceOf[DefaultContext]
    def update(ctx: DefaultContext): Unit = state = state ++ store.update(substate((ctx.reads ++ ctx.writes).keySet))

    private def substate(ids: Set[ID]): ST = ids.map(id => (id, get(id))).toMap
    private def hit(m: MV, id: ID, v: Long): MV = m + (id -> (m.getOrElse(id, Long.MaxValue) min v))

    def previous: Context = previous_
    def failure: Failure = failure_

    def commit(): Option[CommitFailure] = {
      val result = store.commit(reads, writes, sends)

      if (result.isEmpty) {
        previous_ = this.clone.asInstanceOf[DefaultContext]
        reads = Map()
        writes = Map()
        sends = Vector()
        failure_ = null
      }

      result
    }

    private def get[O](id: Id[O]): VObject[O] = {
      // not a pure get, side-effects state
      if (state.contains(id)) state(id).asInstanceOf[VObject[O]]
      else {
        state = state ++ store.update(Map(id -> VObject(0, id.init)))
        get(id)
      }
    }
    
    def apply[O](id: Id[O]): VObject[O] = {
      val vobj = get(id)
      reads = hit(reads, id, vobj.version)
      vobj
    }

    def send[O, I <: Id[O], R](id: I, message: Message[O, I, R]): R = {
      val old = get(id)
      val vid_old = VId(old.version, id)

      val new_context = copyThis()
      val previous = copyThis()

      new_context.previous_ = null
      new_context.level = level + 1
      new_context.sends = Vector()

      // inject/scope new Context and 'this' into Message
      message.thisVar = id
      message.contextVar = new_context

      try {
        if (message.pre) {

          val new_self = message.app

          val send = {
            if (new_self == old.obj) ReadSend(level, vid_old, message)
            else WriteSend(level, vid_old, message)
          }

          new_context.state = new_context.state + (id -> VObject(old.version + 1, new_self))
          new_context.writes = hit(new_context.writes, id, old.version)

          val result = message.eff

          // inject previous context for post condition
          new_context.previous_ = previous
          
          if (message.pst) {
            sends = (sends :+ send) ++ new_context.sends

            state = new_context.state
            reads = new_context.reads
            writes = new_context.writes
            previous_ = previous

            result
          }
          else throw FailureException(PostFailed(vid_old, id.obj(new_context), message))
        }
        else throw FailureException(PreFailed(vid_old, id.obj(new_context), message))
      }
      catch {
        case e: Throwable => {
          e match {
            case FailureException(f) => failure_ = f
            case _ => failure_ = ExceptionFailure(e)
          }

          reads = new_context.reads
          writes = new_context.writes
          sends = (sends :+ FailureSend(level, vid_old, message)) ++ new_context.sends
          previous_ = previous

          throw e
        }
      }
    }
  }

  case class VId[+O](version: Long, id: Id[O])

  trait Send[+O, I <: Id[O], +R] {
    def level: Int
    def vid: VId[O]
    def message: Message[O, I, R]
  }

  case class ReadSend[+O, I <: Id[O], +R](level: Int, vid: VId[O], message: Message[O, I, R]) extends Send[O, I, R]
  case class WriteSend[+O, I <: Id[O], +R](level: Int, vid: VId[O], message: Message[O, I, R]) extends Send[O, I, R]
  case class FailureSend[+O, I <: Id[O], +R](level: Int, vid: VId[O], message: Message[O, I, R]) extends Send[O, I, R]

  type STYPE = Send[Any, _ <: Id[Any] , Any]
  
  case class PreFailed[+O, I <: Id[O], +R](id: VId[O], state: O, message: Message[O, I, R]) extends Failure
  case class PostFailed[+O, I <: Id[O], +R](id: VId[O], state: O, message: Message[O, I, R]) extends Failure

  trait CommitFailure
  case class StoreFailure() extends CommitFailure
  case class SnapshotFailure(snapshot: Map[Id[_], Long]) extends CommitFailure

  case class WriteFailureException(f: CommitFailure) extends Exception {
    override def toString = "WriteFailureException(" + f + ")"
  }

  trait Store {
    def update(state: Map[Id[_], VObject[_]]): Map[Id[_], VObject[_]]
    def commit(reads: Map[Id[_], Long], writes: Map[Id[_], Long], sends: Vector[STYPE]): Option[CommitFailure]
  }
}
