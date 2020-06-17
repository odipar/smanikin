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
    var state: ST = Map()
    var reads_ : MV = Map()
    var writes_ : MV = Map()
    var sends: Vector[SEND] = Vector()

    def copyThis(): DefaultContext = this.clone().asInstanceOf[DefaultContext]
    def update(ctx: DefaultContext): Boolean = {
      val old = sub_state((ctx.reads_ ++ ctx.writes_).keySet)
      val updated = store.update(old)
      state = state ++ updated
      old != updated   // return whether anything got updated
    }

    private def sub_state(ids: Set[ID]): ST = ids.map(id => (id, get(id))).toMap
    private def hit(m: MV, id: ID, v: Long): MV = m + (id -> (m.getOrElse(id, Long.MaxValue) min v))

    def previous: Context = previous_
    def failure: Failure = failure_

    def commit(): Unit = {
      val result = store.commit(reads_, writes_, sends)

      if (result.isEmpty) {
        previous_ = this.clone.asInstanceOf[DefaultContext]
        reads_ = Map()
        writes_ = Map()
        sends = Vector()
        failure_ = null
      }
      else throw CommitFailureException(result.get)
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
      reads_ = hit(reads_, id, vobj.version)
      vobj
    }

    def send[O, I <: Id[O], R](id: I, message: Message[O, I, R]): R = {
      val old = get(id)
      val vid_old = VId(old.version, id)

      val previous = copyThis()
      val new_context = copyThis()

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
          new_context.writes_ = hit(new_context.writes_, id, old.version)

          val result = message.eff

          // inject previous context for post condition
          new_context.previous_ = previous

          if (message.pst) {
            sends = (sends :+ send) ++ new_context.sends

            state = new_context.state
            reads_ = new_context.reads_
            writes_ = new_context.writes_
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

          reads_ = new_context.reads_
          writes_ = new_context.writes_
          sends = (sends :+ FailureSend(level, vid_old, message)) ++ new_context.sends
          previous_ = previous

          throw e
        }
      }
    }
  }
  
  case class PreFailed[+O, I <: Id[O], +R](id: VId[O], state: O, message: Message[O, I, R]) extends Failure
  case class PostFailed[+O, I <: Id[O], +R](id: VId[O], state: O, message: Message[O, I, R]) extends Failure
}
