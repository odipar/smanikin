package net.manikin.core.context

object TestContext {
  import net.manikin.core.TransObject.{Context, FailureException, Id, Message, MessageContext, VObject}
  import net.manikin.core.context.DefaultContext.{PostFailed, PreFailed}
  import net.manikin.core.context.Store._

  case class TestContext() extends Context {
    var prev: Map[Id[_], VObject[_]] = Map()
    var current: Map[Id[_], VObject[_]] = Map()

    def state: Map[Id[_], VObject[_]] = current
    def withState(m: Map[Id[_], VObject[_]]): TestContext = { prev = m ; current = m ; this }
    
    def apply[O](id: Id[O]): VObject[O] = current.getOrElse(id, VObject(0, id.init)).asInstanceOf[VObject[O]]
    def previous[O](id: Id[O]): VObject[O] = prev.getOrElse(id, VObject(0, id.init)).asInstanceOf[VObject[O]]
    def send[O, R](id: Id[O], message: Message[Id[O], O, R]): R = {
      val old_obj = apply(id)
      val old_vid = VId(old_obj.version, id)

      // inject/scope MessageContext into Message
      message.msgContext = MessageContext(id, this)

      if (!message.pre) throw FailureException(PreFailed(old_vid, id.obj(this), message))
      else {
        val new_self = message.app

        val new_obj = VObject(old_obj.version + 1 , new_self)
        current = current + (id -> new_obj)

        try {
          val result = message.eff

          prev = prev + (id -> old_obj) // restore the old version (could have been overwritten by the recursive eff)

          if (!message.pst) throw FailureException(PostFailed(old_vid, id.obj(this), message))
          else {
            prev = prev + (id -> new_obj)
            result
          }
        }
        catch {
          case t: Throwable => {
            prev = prev + (id -> old_obj) // rollback
            current = current + (id -> old_obj)
            throw t
          }
        }
      }
    }

    def retries = 0
  }
}
