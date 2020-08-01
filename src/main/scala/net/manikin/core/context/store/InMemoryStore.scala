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

          val evt = snap.getOrElse(id, Map())

          // replay
          while (evt.contains(v_obj.version)) {
            val snd = evt(v_obj.version)
            val msg = snd.message

            // inject replay context and this
            msg.msgContext = MessageContext(id, ReplayContext(id, v_obj))

            // apply event
            v_obj = VObject(v_obj.version + 1, snd.vid.serial_id, msg.app)
          }

          (id, v_obj)
        }
      }
    }

    val empty = Map[Long, SEND]()

    def commit(reads: MV, sends: Seq[SEND]): Unit = {
      this.synchronized { // atomic

        var snap = events;

        // check if the snapshot has been invalidated by other writes
        reads.foreach {
          r => if (snap.getOrElse(r._1, empty).contains(r._2)) throw FailureException(SnapshotFailure())
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
      }
    }
  }
}
