package net.manikin.core.context

object DefaultContext {
  import Store._
  import net.manikin.core.TransObject._
  import net.manikin.core.context.store.InMemoryStore._
  import scala.collection.immutable.HashMap

  // A DefaultContext keeps track of (historical) Object states and Message dispatches
  // If an Object cannot be found via its Id, it will be fetched from the backing Store
  case class DefaultContext(private val store: Store = new InMemoryStore()) extends Context with Cloneable {
    private var level = 0
    private var retries_ = 0
    private var state: ST = HashMap()
    private var prev: ST = HashMap()
    private var current: ST = HashMap()
    private var sends: Vector[SEND] = Vector()

    def retries = retries_
    def copyThis(): DefaultContext = this.clone().asInstanceOf[DefaultContext]
    def retry(ctx: DefaultContext): Boolean = {
      val old = ctx.sub_state
      val updated = store.update(old)
      state = state ++ updated
      retries_ += 1
      old != updated   // return whether anything got updated (no retries needed if not)
    }

    private def sub_state: ST = prev.keySet.map(id => (id, state(id))).toMap

    def commit(): Unit = {
      val write_origins = sends.groupBy(x => x.vid.id).map(x => (x._1, x._2.map(x => x.vid.version).min))
      // prev versions are superseded by write origin versions
      store.commit(prev.map(x => (x._1, x._2.version)) ++ write_origins, sends)
      
      state = state ++ current
      prev = HashMap()
      current = HashMap()
      sends = Vector()
      retries_ = 0
    }

    private def v[O](v: VObject[_]): VObject[O] = v.asInstanceOf[VObject[O]]
    private def updateFromStore[O](id: Id[O]): VObject[O] = {
      val upd = store.update(Map(id -> state.getOrElse(id, VObject(0, id.init))))
      prev = prev ++ upd
      v(upd(id))
    }
    
    def previous[O](id: Id[O]): VObject[O] = v(prev.getOrElse(id, updateFromStore(id)))
    def apply[O](id: Id[O]): VObject[O] = v(current.getOrElse(id, previous(id)))
    
    def send[O, I <: Id[O], R](id: I, message: Message[O, I, R]): R = {
      val old = apply(id)
      val vid_old = VId(old.version, id)

      // inject/scope new Context and 'this' into Message
      message.msgContext = MessageContext(id, this)

      val oldSends = sends
      sends = Vector[SEND]()

      if (!message.pre) throw FailureException(PreFailed(vid_old, id.obj(this), message))
      else {
        val new_self = message.app

        val send = {
          if (new_self == old.obj) ReadSend(level, vid_old, message)
          else WriteSend(level, vid_old, message)
        }

        val vobject = VObject(old.version + 1, new_self)
        current = current + (id -> vobject)

        level = level + 1
        val result = message.eff
        level = level - 1
        
        prev = prev + (id -> old) // reset the old version (could have been changed by the recursive eff)

        if (!message.pst) throw FailureException(PostFailed(vid_old, id.obj(this), message))
        else {
          sends = (oldSends :+ send) ++ sends
          prev = prev + (id -> vobject)
          result
        }
      }
    }
  }

  case class PreFailed[+O, I <: Id[O], +R](id: VId[O], state: O, message: Message[O, I, R]) extends Failure
  case class PostFailed[+O, I <: Id[O], +R](id: VId[O], state: O, message: Message[O, I, R]) extends Failure
}
