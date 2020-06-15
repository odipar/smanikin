package net.manikin.core.context

object Transactor {
  import net.manikin.core.TransObject._
  import net.manikin.core.context.DefaultContext._

  case class Transactor(private var ctx: DefaultContext = DefaultContext()) {
    def apply[O](id: Id[O]): VObject[O] = ctx.apply(id)
    def commit[O, I <: Id[O], R](id: I, message: Message[O, I, R]): R = commit(id, message, 3)
    def commit[O, I <: Id[O], R](id: I, message: Message[O, I, R], retry: Int): R = {
      val new_context = ctx.copyThis()

      try {
        val result = new_context.send(id, message)
        new_context.commit()
        ctx = new_context
        result
      }
      catch {
        case e: Throwable => {
          ctx.update(new_context)
          if (retry > 0) commit(id, message, retry - 1)
          else throw e
        }
      }
    }
  }  
}