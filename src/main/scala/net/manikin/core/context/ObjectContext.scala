package net.manikin.core.context

object ObjectContext {
  import net.manikin.core.TransObject.{Context, FailureException, Id, Message, MessageContext, VObject}
  import net.manikin.core.context.EventContext.{PostFailed, PreFailed}
  import net.manikin.core.context.Store._
  import net.manikin.core.TransObject.ST
  import scala.collection.immutable.HashMap

  class ObjectContext() extends Context {
    var prev: ST = HashMap()
    var current: ST = HashMap()
    var failures = HashMap[Id[_], Int]()

    def state: Map[Id[_], VObject[_]] = current
    def withState(m: Map[Id[_], VObject[_]]): ObjectContext = {
      failures = HashMap()
      prev = Map()
      current = m
      this
    }

    def latest[O](id: Id[O]): VObject[O] = VObject(0, 0, id.init)
    def apply[O](id: Id[O]): VObject[O] = current.getOrElse(id, previous(id)).asInstanceOf[VObject[O]]
    def previous[O](id: Id[O]): VObject[O] = prev.getOrElse(id, latest(id)).asInstanceOf[VObject[O]]

    def send[O, R](id: Id[O], message: Message[Id[O], O, R]): R = {
      val old_obj = apply(id)
      val old_vid = VId(old_obj.version, old_obj.serial_id, id)

      // inject/scope MessageContext into Message
      message.msgContext = MessageContext(id, this)

      try {
        if (!message.pre) throw FailureException(PreFailed(old_vid, id.obj(this), message))
        else {
          val new_self = message.app

          val new_obj = VObject(old_obj.version + 1, old_obj.serial_id, new_self)

          prev = prev + (id -> old_obj)
          current = current + (id -> new_obj)

          val result = message.eff

          prev = prev + (id -> old_obj) // we need to inject the old_obj again because eff could have overwritten it

          if (!message.pst) throw FailureException(PostFailed(old_vid, id.obj(this), message))
          else result
        }
      }
      catch {
        case t: Throwable => {
          failures = failures + (id -> failures.getOrElse(id, 0))
          // rollback
          prev = prev + (id -> old_obj)
          current = current + (id -> old_obj)
          throw t
        }
      }
    }

    def failures[O](id: Id[O]) = failures.getOrElse(id, 0)
  }
}
