package net.manikin.core.context

object Transactor {
  import net.manikin.core.TransObject._
  import net.manikin.core.context.StoreContext._
  import java.util.UUID

  // A Transactor commits to a StoreContext. Upon failure it retries.
  case class Transactor(private var ctx: StoreContext = new StoreContext()) {
    def apply[O](id: Id[O]): VObject[O] = ctx.apply(id)
    def commit[I <: Id[O], O, R](id: I, message: Message[I, O, R]): R = commit(id, message, 3)
    def commit[I <: Id[O], O, R](id: I, message: Message[I, O, R], retry: Int): R = {
      val new_context = ctx.copyThis()

      try {
        val result = new_context.send(id, message)
        new_context.commit()
        ctx = new_context
        result
      }
      catch {
        case e: Throwable => {
          if (retry > 0) {
            val different = ctx.retry(new_context)
            if (ctx.failures(id) > 1 && !different) throw e
            commit(id, message, retry - 1)
          }
          else throw e
        }
      }
    }
  }

  case class TId(uuid: UUID = UUID.randomUUID()) extends Id[Unit] { def init: Unit = { } }

  trait Transaction[+R] extends Message[TId, Unit, R] {
    def pre = true
    def app = { }
    def pst = true
  }
}