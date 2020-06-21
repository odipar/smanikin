package net.manikin.core.context

object DefaultContext {
  import Store._
  import net.manikin.core.TransObject._
  import net.manikin.core.context.store.InMemoryStore._

  // A DefaultContext keeps track of (historical) Object states and Message dispatches
  // If an Object cannot be found via its Id, it will be fetched from the backing Store
  case class DefaultContext(private val store: Store = new InMemoryStore()) extends Context with Cloneable {
    private var level = 0
    private var failure_ : Failure = _
    private var previous_ : DefaultContext = _
    private var state: ST = Map()
    private var reads : ST = Map()
    private var writes : ST = Map()
     var sends: Vector[SEND] = Vector()

    def copyThis(): DefaultContext = this.clone().asInstanceOf[DefaultContext]
    def update(ctx: DefaultContext): Boolean = {
      val old = ctx.sub_state
      val updated = store.update(old)
      state = state ++ updated
      old != updated   // return whether anything got updated (no retries needed if not)
    }

    private def sub_state: ST = reads.keySet.map(id => (id, state(id))).toMap

    def previous: Context = previous_
    def failure: Failure = failure_

    def commit(): Unit = {
      val write_origins = sends.groupBy(x => x.vid.id).map(x => (x._1, x._2.map(x => x.vid.version).min))
      val result = store.commit(reads.map(x => (x._1, x._2.version)) ++ write_origins, sends)

      if (result.isEmpty) {
        state = state ++ writes
        reads = Map()
        writes = Map()
        sends = Vector()
        failure_ = null
      }
      else throw CommitFailureException(result.get)
    }

    def previous[O](id: Id[O]): VObject[O] = {
      if (reads.contains(id)) reads(id).asInstanceOf[VObject[O]]
      else updateFromStore(id)
    }

    def apply[O](id: Id[O]): VObject[O] = {
      if (writes.contains(id)) writes(id).asInstanceOf[VObject[O]]
      else if (reads.contains(id)) reads(id).asInstanceOf[VObject[O]]
      else updateFromStore(id)
    }

    private def updateFromStore[O](id: Id[O]): VObject[O] = {
      val update = store.update(Map(id -> state.getOrElse(id, VObject(0, id.init))))
      reads = reads ++ update
      state = state ++ update
      apply(id)
    }
    
    def send[O, I <: Id[O], R](id: I, message: Message[O, I, R]): R = {
      val old = apply(id)
      val vid_old = VId(old.version, id)

      // inject/scope new Context and 'this' into Message
      message.thisVar = id
      message.contextVar = this

      val oldSends = sends
      sends = Vector[SEND]()

      try {
        if (message.pre) {

          val new_self = message.app

          val send = {
            if (new_self == old.obj) ReadSend(level, vid_old, message)
            else WriteSend(level, vid_old, message)
          }

          writes = writes + (id -> VObject(old.version + 1, new_self))

          level = level + 1
          val result = message.eff
          level = level - 1

          if (message.pst) {
            sends = (oldSends :+ send) ++ sends
            reads = reads ++ writes
            result
          }
          else throw FailureException(PostFailed(vid_old, id.obj(this), message))
        }
        else throw FailureException(PreFailed(vid_old, id.obj(this), message))
      }
      catch {
        case e: Throwable => {
          e match {
            case FailureException(f) => failure_ = f
            case _ => failure_ = ExceptionFailure(e)
          }

          sends = (oldSends :+ FailureSend(level, vid_old, message)) ++ sends

          throw e
        }
      }
    }
  }

  case class PreFailed[+O, I <: Id[O], +R](id: VId[O], state: O, message: Message[O, I, R]) extends Failure
  case class PostFailed[+O, I <: Id[O], +R](id: VId[O], state: O, message: Message[O, I, R]) extends Failure
}
