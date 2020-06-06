package net.manikin.core

object TransactionContext {
  import TransactionalObject._

  // Simple TransactionContext that keeps track of (historical) Object states and Message dispatches
  class TransactionContext() extends Context with Cloneable {
    private var failure: Failure = null
    private var level = 0
    private var previousContext: Context = _
    private var vStateMap: Map[Id[_], Long] = Map()
    private var stateMap: Map[Id[_], _] = Map()
    private var reads: Set[VId[_]] = Set()
    private var writes: Set[VId[_]] = Set()
    private var sends: Vector[Send[_, _, _]] = Vector()

    def previous: Context = previousContext
    def sent: Vector[Send[_, _, _]] = sends
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

    def send[X, I <: Id[X], R](id: I, message: Message[X, I, R]): R = {
      val vid = VId[X](id, version(id))
      val previous_context = this.clone.asInstanceOf[TransactionContext]

      // inject Context and 'self' into Message so it will be in scope
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
            val s =  Send(level, vid, message)
            sends = (previous_context.sends :+ s)  ++ sends
            result
          }
          else {
            failure = PostFailed(vid, id()(this), message)
            throw FailureException(failure)
          }
        }
        else {
          failure = PreFailed(vid, id()(this), message)
          throw FailureException(failure)
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

  case class VId[+X](id: Id[X], version: Long) {
    override def toString: String = version + ":" + id
  }

  case class Send[+X, I <: Id[X], +R](level: Int, vid: VId[X], message: Message[X, I, R])
  case class PreFailed[+X, I <: Id[X], +R](id: VId[X], state: X, message: Message[X, I, R]) extends Failure
  case class PostFailed[+X, I <: Id[X], +R](id: VId[X], state: X, message: Message[X, I, R]) extends Failure

}
