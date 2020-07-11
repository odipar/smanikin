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

    protected def vobj[O](v: VObject[_]): VObject[O] = v.asInstanceOf[VObject[O]]
    protected def latestVersion[O](id: Id[O]): VObject[O] = VObject(0, id.init)

    def previous[O](id: Id[O]): VObject[O] = vobj(prev.getOrElse(id, latestVersion(id)))
    def apply[O](id: Id[O]): VObject[O] = vobj(current.getOrElse(id, previous(id)))

    def send[O, R](id: Id[O], message: Message[Id[O], O, R]): R = {
      val old_obj = apply(id)
      val old_vid = VId(old_obj.version, id)

      // inject/scope MessageContext into Message
      message.msgContext = MessageContext(id, this)

      val oldSends = sends
      sends = Vector()

      if (!message.pre) throw FailureException(PreFailed(old_vid, id.obj(this), message))
      else {
        val new_self = message.app

        val send = {
          if (new_self == old_obj.obj) ReadSend(level, old_vid, message)
          else WriteSend(level, old_vid, message)
        }.asInstanceOf[SEND]

        val new_obj = VObject(old_obj.version + 1, new_self)
        current = current + (id -> new_obj)

        level = level + 1
        val result = message.eff
        level = level - 1

        prev = prev + (id -> old_obj) // restore the old version (could have been overwritten by the recursive eff)

        if (!message.pst) throw FailureException(PostFailed(old_vid, id.obj(this), message))
        else {
          sends = (oldSends :+ send) ++ sends
          prev = prev + (id -> new_obj)
          result
        }
      }
    }

    def retries = 0
  }

  case class PreFailed[+I <: Id[O], O, +R](id: VId[O], state: O, message: Message[I, O, R]) extends Failure
  case class PostFailed[+I <: Id[O], O, +R](id: VId[O], state: O, message: Message[I, O, R]) extends Failure
}
