package net.manikin.core.context

object DefaultContext {
  import Store._
  import net.manikin.core.TransObject._
  import scala.collection.immutable.HashMap

  // A DefaultContext keeps track of (historical) Object states and Message dispatches
  class DefaultContext extends Context {
    protected var level = 0
    protected var prev: ST = HashMap()
    protected var current: ST = HashMap()
    protected var sends: Vector[SEND] = Vector()

    protected def v[O](v: VObject[_]): VObject[O] = v.asInstanceOf[VObject[O]]
    protected def latestVersion[O](id: Id[O]): VObject[O] = VObject(0, id.init)

    def previous[O](id: Id[O]): VObject[O] = v(prev.getOrElse(id, latestVersion(id)))
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

    def retries = 0
  }

  case class PreFailed[+O, I <: Id[O], +R](id: VId[O], state: O, message: Message[O, I, R]) extends Failure
  case class PostFailed[+O, I <: Id[O], +R](id: VId[O], state: O, message: Message[O, I, R]) extends Failure
}
