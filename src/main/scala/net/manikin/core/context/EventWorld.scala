package net.manikin.core.context

object EventWorld {
  import Store._
  import net.manikin.core.TransObject._
  import scala.collection.immutable.HashMap

  // A EventWorld keeps track of (historical) Object states and Message dispatches
  class EventWorld extends World {
    protected var level = 0
    protected var prev: ST = HashMap()
    protected var current: ST = HashMap()
    protected var sends: Vector[SEND] = Vector()
    protected var failures_ = HashMap[Id[_], Int]()

    protected def vobj[O](v: VObject[_]): VObject[O] = v.asInstanceOf[VObject[O]]
    protected def latestVersion[O](id: Id[O]): VObject[O] = VObject(0, 0, id.init)

    def previous[O](id: Id[O]): VObject[O] = vobj(prev.getOrElse(id, latestVersion(id)))
    def apply[O](id: Id[O]): VObject[O] = vobj(current.getOrElse(id, previous(id)))

    def commit[O, R](id: Id[O], message: Message[Id[O], O, R]): R = {
      val vobj = apply(id)
      if (vobj.version > 0 && (vobj.version % 10) == 0) { // snapshot after 100 events
        send2(id, Snapshot(vobj.obj))
        send2(id, message)
      }
      else send2(id, message)
    }
    def send2[O, R](id: Id[O], message: Message[Id[O], O, R]): R = {
      val old_obj = apply(id)
      val old_vid = VId(old_obj.version, old_obj.serial_id, id)

      // inject/scope MessageContext into Message
      message.msgContext = MessageContext(id, this)

      val oldSends = sends
      val oldLevel = level

      sends = Vector()

      try {
        if (!message.pre) throw FailureException(PreFailed(old_vid, id.obj(this), message))
        else {
          val new_self = message.app

          val send = {
            if (new_self == old_obj.obj) ReadSend(level, old_vid, message)
            else WriteSend(level, old_vid, message)
          }.asInstanceOf[SEND]

          val new_obj = VObject(old_obj.version + 1, old_obj.serial_id, new_self)

          prev = prev + (id -> old_obj)
          current = current + (id -> new_obj)

          level = level + 1
          val result = message.eff
          level = oldLevel

          prev = prev + (id -> old_obj)

          if (!message.pst) throw FailureException(PostFailed(old_vid, id.obj(this), message))
          else {
            sends = (oldSends :+ send) ++ sends
            result
          }
        }
      }
      catch {
        case t: Throwable => {
          failures_ = failures_ + (id -> failures_.getOrElse(id, 0))

          // rollback
          level = oldLevel
          sends = oldSends
          prev = prev + (id -> old_obj)
          current = current + (id -> old_obj)

          throw t
        }
      }
    }

    def failures[O](id: Id[O]): Int = failures_.getOrElse(id, 0)
  }

  case class PreFailed[+I <: Id[O], O, +R](id: VId[O], state: O, message: Message[I, O, R]) extends Failure
  case class PostFailed[+I <: Id[O], O, +R](id: VId[O], state: O, message: Message[I, O, R]) extends Failure
}
