package net.manikin.core

object DefaultContext {
  import TransactionalObject._

  // A DefaultContext keeps track of (historical) Object states and Message dispatches
  class DefaultContext() extends Context with Cloneable {
    private var failure_ : Failure = null
    private var level = 0
    private var previousContext: DefaultContext = null
    private var stateMap: Map[Id[_], VObject[_]] = Map()
    var reads: Map[Id[_], Long] = Map()
    var writes: Map[Id[_], Long] = Map()
    private var sends: Vector[STYPE] = Vector()

    def snapshot: Map[Id[_], Long] = reads ++ writes
    def previous: Context = previousContext
    def previousDefaultContext: DefaultContext = previousContext

    def withFailure(f: Failure): Unit = failure_ = f
    def failure: Failure = failure_

    def update(updates: Map[Id[_], VObject[_]]): Unit = {
      stateMap = stateMap ++ updates
    }

    def commit(s: Store): Option[CommitFailure] = {
      val result = s.commit(reads, writes, sends)

      if (result.isEmpty) {
        val prev = this.clone.asInstanceOf[DefaultContext]
        reads = Map()
        writes = Map()
        sends = Vector()
        failure_ = null
        previousContext = prev
      }

      result
    }

    def apply[O](id: Id[O]): VObject[O] = {
      val vobj = stateMap.getOrElse(id, VObject(0, id.init))
      reads = reads + (id -> (reads.getOrElse(id, Long.MaxValue) min vobj.version))
      vobj.asInstanceOf[VObject[O]]
    }

    def copyOf(): DefaultContext = this.clone().asInstanceOf[DefaultContext]
    
    def send[O, I <: Id[O], R](id: I, message: Message[O, I, R]): R = {
      val old = stateMap.getOrElse(id, VObject(0, id.init))
      val vid = VId(old.version, id)

      val new_context = copyOf()

      new_context.previousContext = this
      new_context.level = level + 1
      new_context.sends = Vector()

      // inject/scope new Context and 'this' into Message
      message.thisVar = id
      message.contextVar = new_context

      try {
        if (message.pre) {

          val new_self = message.app

          // New version
          if (new_self != old.obj) {
            val nwrites = new_context.writes
            new_context.stateMap = new_context.stateMap + (id -> VObject(old.version + 1, new_self))
            new_context.writes = nwrites + (id -> (nwrites.getOrElse(id, Long.MaxValue) min old.version))
          }

          val result = message.eff

          if (message.pst) {
            
            stateMap = new_context.stateMap
            reads = new_context.reads
            writes = new_context.writes
            sends = (sends :+ Send(level, VId(new_context(id).version, id), message)) ++ new_context.sends
            
            result
          }
          else {
            failure_ = PostFailed(vid, id.obj(this), message)
            throw FailureException(failure_)
          }
        }
        else {
          failure_ = PreFailed(vid, id.obj(this), message)
          throw FailureException(failure_)
        }
      }
      catch {
        case e: Throwable => {
          
          reads = new_context.reads
          writes = new_context.writes

          if (failure_ == null) failure_ = ExceptionFailure(e, new_context.reads ++ new_context.writes)
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
    def update(s: Map[Id[_], VObject[_]]): Map[Id[_], VObject[_]]
    def commit(reads: Map[Id[_], Long], writes: Map[Id[_], Long], sends: Vector[STYPE]): Option[CommitFailure]
  }
}
