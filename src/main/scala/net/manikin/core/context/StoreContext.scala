package net.manikin.core.context

object StoreContext {
  import Store._
  import net.manikin.core.TransObject._
  import net.manikin.core.context.store.InMemoryStore._
  import net.manikin.core.context.EventContext._
  import scala.collection.immutable.HashMap

  // A StoreContext keeps track of (historical) Object states and Message dispatches
  // If an Object cannot be found via its Id, it will be fetched from the backing Store
  class StoreContext(private val store: Store = new InMemoryStore()) extends EventContext with Cloneable {
    protected var retries_ = 0
    protected var state: ST = HashMap()

    def copyThis(): StoreContext = this.clone().asInstanceOf[StoreContext]
    def retry(ctx: StoreContext): Boolean = {
      val old = ctx.prev.keySet.map(id => (id, ctx.state(id))).toMap
      val updated = store.update(old)
      state = state ++ updated
      retries_ += 1
      old != updated // return whether anything got updated (no retries needed if not)
    }

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


    override protected def latestVersion[O](id: Id[O]): VObject[O] = {
      val upd = store.update(Map(id -> state.getOrElse(id, VObject(0, id.init))))
      prev = prev ++ upd
      state = state ++ upd
      previous(id)
    }
  }
}
