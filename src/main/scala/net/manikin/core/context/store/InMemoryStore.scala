package net.manikin.core.context.store

object InMemoryStore {
  import net.manikin.core.context.Store._
  import net.manikin.core.TransObject._

  class InMemoryStore extends Store {
    var events: Map[ID, Map[Long, SEND]] = Map()

    // 'Updates' the state to the most recent state
    def update(state: ST): ST = {
      state.map { x =>
        val id = x._1
        var v_obj = x._2

        var version = v_obj.version
        val evt = events.getOrElse(id, Map())

        // replay
        while(evt.contains(version)) {
          val msg = evt(version).message

          // inject replay context and this
          msg.thisVar = id
          msg.contextVar = ReplayContext(id, VObject(version, v_obj.obj))

          // apply event
          version += 1
          v_obj = VObject(version, msg.app)
        }

        (id, v_obj)
      }
    }

    val empty = Map[Long, SEND]()

    def commit(reads: MV, writes: MV, sends: Vector[SEND]): Option[StoreFailure] = {
      this.synchronized { // atomic
        val rw = reads ++ writes
        
        rw.foreach {
          r => if (events.getOrElse(r._1, empty).contains(r._2 + 1)) return Some(SnapshotFailure(rw))
        }

        sends.foreach { s =>
          val vid = s.vid
          val evts = events.getOrElse(vid.id, Map()) + (vid.version -> s)
          events = events + (vid.id -> evts)
        }

        None
      }
    }
  }
}
