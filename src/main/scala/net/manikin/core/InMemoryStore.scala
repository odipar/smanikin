package net.manikin.core

object InMemoryStore {
  import net.manikin.core.DefaultContext._
  import net.manikin.core.TransactionalObject._

  class InMemoryStore extends Store {
    var events: Map[Id[_], Map[Long, STYPE]] = Map()
    
    def update(state: Map[Id[_], VObject[_]]): Map[Id[_], VObject[_]] = {
      state.map { x =>
        val id = x._1
        var v_obj = x._2

        var version = v_obj.version + 1
        val evt = events.getOrElse(id, Map())

        while(evt.contains(version)) {
          val msg = evt(version).message

          // inject replay context and this
          msg.thisVar = id
          msg.contextVar = ReplayContext(id, VObject(version, v_obj.obj))

          // apply event
          v_obj = VObject(version, msg.app)
          version += 1
        }

        (id, v_obj)
      }
    }

    val empty = Map[Long, STYPE]()

    def commit(reads: Map[Id[_], Long], writes: Map[Id[_], Long], sends: Vector[STYPE]): Option[CommitFailure] = {
      println("reads: " + reads)
      println("writes: " + writes)
      println("commit: " + sends)

      this.synchronized { // atomic
        if (writes.nonEmpty) {
          // if there are writes, check snapshot of both reads and writes (Serializability)
          (reads ++ writes).foreach {
            r => {
              if (events.getOrElse(r._1, empty).contains(r._2 + 1)) return Some(SnapshotFailure(reads ++ writes))
            }
          }
        }

        sends.foreach { s =>
          val vid = s.vid
          val evts = events.getOrElse(vid.id, Map()) + (vid.version -> s)
          events = events + (vid.id -> evts)
        }

        None
      }
    }

    private case class ReplayContext(sid: Id[_], vobject: VObject[_]) extends Context {
      def apply[O](id: Id[O]): VObject[O] = { if (sid == id) vobject.asInstanceOf[VObject[O]] ; else error }
      def send[O, I <: Id[O], R](id: I, message: Message[O, I, R]): R = error
      def withFailure(f: Failure): Unit = error
      def failure: Failure = null
      def previous: Context = error
      def error = sys.error("replaying")
    }
  }
}
