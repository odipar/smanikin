package net.manikin.core

object DefaultContext {
  import TransactionalObject._

  // A DefaultContext keeps track of (historical) Object states and Message dispatches
  class DefaultContext() extends Context with Cloneable {
    private var failure_ : Failure = null
    private var level = 0
    private var previousContext: DefaultContext = null
    var stateMap: Map[Id[_], VObject[_]] = Map()
    var reads: Set[Id[_]] = Set()
    var writes: Set[Id[_]] = Set()
    var sends: Vector[STYPE] = Vector()

    private def substate(ids: Set[Id[_]]): Map[Id[_], VObject[_]] = {
      ids.map(id => (id, stateMap.getOrElse(id, VObject(0, id.init)))).toMap
    }

    private def versions(ids: Set[Id[_]]): Map[Id[_], Long] = {
      substate(ids).map(x => (x._1, x._2.version))
    }
    
    def snapshot: Map[Id[_], VObject[_]] = substate(reads ++ writes)
    def previous: Context = previousContext
    def previousDefaultContext: DefaultContext = previousContext

    def withFailure(f: Failure): Unit = failure_ = f
    def failure: Failure = failure_
    def update(updates: Map[Id[_], VObject[_]]): Unit = stateMap = stateMap ++ updates

    def commit(s: Store): Option[CommitFailure] = {
      val result = s.commit(versions(reads), versions(writes), sends)

      if (result.isEmpty) {
        val prev = this.clone.asInstanceOf[DefaultContext]
        reads = Set()
        writes = Set()
        sends = Vector()
        failure_ = null
        previousContext = prev
      }

      result
    }

    def apply[O](id: Id[O]): VObject[O] = {
      val vobj = stateMap.getOrElse(id, VObject(0, id.init))
      reads = reads + id
      vobj.asInstanceOf[VObject[O]]
    }

    def copyThis(): DefaultContext = this.clone().asInstanceOf[DefaultContext]

    def send[O, I <: Id[O], R](id: I, message: Message[O, I, R]): R = {
      val old = stateMap.getOrElse(id, VObject(0, id.init))
      val vid = VId(old.version, id)

      val new_context = copyThis()
      val previous = copyThis()
      
      new_context.previousContext = this
      new_context.level = level + 1
      new_context.sends = Vector()

      // inject/scope new Context and 'this' into Message
      message.thisVar = id
      message.contextVar = new_context

      try {
        if (message.pre) {

          val new_self = message.app

          if (new_self != old.obj) {
            new_context.stateMap = new_context.stateMap + (id -> VObject(old.version + 1, new_self))
            new_context.writes = new_context.writes + id
          }

          val result = message.eff

          new_context.previousContext = previous

          if (message.pst) {
            previousContext = previous

            stateMap = new_context.stateMap
            reads = new_context.reads
            writes = new_context.writes
            sends = (sends :+ Send(level, VId(new_context(id).version, id), message)) ++ new_context.sends

            result
          }
          else throw FailureException(PostFailed(vid, id.obj(this), message))
        }
        else throw FailureException(PreFailed(vid, id.obj(this), message))
      }
      catch {
        case e: Throwable => {
          e match {
            case FailureException(f) => failure_ = f
            case _ => failure_ = ExceptionFailure(e)
          }

          previousContext = previous
          reads = new_context.reads
          writes = new_context.writes
          sends = (sends :+ Send(level, VId(new_context(id).version, id), message)) ++ new_context.sends
          
          throw e
        }
      }
    }
  }

  case class VId[+O](version: Long, id: Id[O])
  case class Send[+O, I <: Id[O], +R](level: Int, vid: VId[O], message: Message[O, I, R])

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
