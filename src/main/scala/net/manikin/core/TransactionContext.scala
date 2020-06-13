package net.manikin.core

import net.manikin.core.InMemoryStore.InMemoryStore

object TransactionContext {
  import net.manikin.core.DefaultContext._
  import net.manikin.core.TransactionalObject._

  case class TransactionContext(
     store: Store = new InMemoryStore(),
     var ctx: DefaultContext = new DefaultContext()) extends Context {

    def apply[O](id: Id[O]): VObject[O] = ctx.apply(id)
    def update(ids: Seq[Id[_]]): Unit = ctx.update(store.update(ids.map(id => (id, ctx(id))).toMap))
    def send[O, I <: Id[O], R](id: I, message: Message[O, I, R]): R = send(id, message, 3)
    def send[O, I <: Id[O], R](id: I, message: Message[O, I, R], retry: Int): R = {
      val prev: DefaultContext = ctx.copyOf()

      try {
        val result = ctx.send(id, message)
        
        ctx.commit(store) match {
          case Some(x) => x match {
            case _ => throw WriteFailureException(x)
          }
          case _ => result
        }
      }
      catch {
        case e: Throwable => {
   
          prev.update(store.update(ctx.snapshot.map(x => (x._1, prev(x._1)))))
          
          ctx = prev
          
          ctx.reads = Map()
          ctx.writes = Map()
          
          if (retry > 0) {
            println(s"retry: $retry")
            send(id, message, retry - 1)
          }
          else {
            throw e
          }
        }
      }
    }
    
    def withFailure(f: Failure): Unit = ctx.withFailure(f)
    def failure: Failure = ctx.failure

    def previous: Context = TransactionContext(store, ctx.previousDefaultContext)
  }
}
