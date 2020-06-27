package net.manikin.core.context.store

object InMemoryStore {
  import net.manikin.core.context.Store._
  import net.manikin.core.TransObject._

  class InMemoryStore extends Store {
    var events: Map[ID, Map[Long, SEND]] = Map()

    // 'Updates' the state to the most recent state
    def update(state: ST): ST = {
       {
        val snap = events
        state.map { x =>
          val id = x._1
          var v_obj = x._2

          var version = v_obj.version
          val evt = snap.getOrElse(id, Map())

          // replay
          while (evt.contains(version)) {  
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
    }

    val empty = Map[Long, SEND]()

    var enter: Int = 0
    
    def commit(reads: MV, sends: Seq[SEND]): Option[StoreFailure] = {
      this.synchronized { // atomic

        var snap = events;

        // check if the snapshot has been invalidated by other writes
        reads.foreach {
          r => {
            if (snap.getOrElse(r._1, empty).contains(r._2)) return Some(SnapshotFailure())
          }
        }
        
        // write events
        sends.foreach { s =>
          val vid = s.vid
          var evts = snap.getOrElse(vid.id, Map())

          assert(!evts.contains(vid.version))

          evts = evts + (vid.version -> s)
          snap = snap + (vid.id -> evts)
        }

        events = snap

        None
      }
    }
  }
}
