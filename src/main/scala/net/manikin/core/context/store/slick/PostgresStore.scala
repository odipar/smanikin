package net.manikin.core.context.store.slick

object PostgresStore {
  import slick.jdbc.PostgresProfile.api._
  import slick.jdbc.TransactionIsolation
  import scala.concurrent.Await
  import scala.concurrent.duration.Duration
  import PostgresTable._
  import net.manikin.core.TransObject._
  import net.manikin.core.context.Store._
  import net.manikin.serialization.SerializationUtils
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.util._
  import SerializationUtils._

  // A Postgres backing Store
  class PostgresStore(config: String = "manikin_db") extends Store {
    val db = Database.forConfig(config)

    def update(state: ST): ST = {
      val buffer = new Array[Byte](16384)

      state.map { x =>
        val idb = toBytes(x._1, buffer)
        var v_obj = x._2

        // replay all events that occurred after the current version of the object
        val eventQuery = event.filter(x => x.id === idb && x.event_id >= v_obj.version).result
        val events = Await.result(db.run(eventQuery), Duration.Inf)

        events.foreach { evt =>
          val id = toObject[Id[Any]](evt._1)
          val msg = toObject[Message[Any, _ <: Id[Any], Any]](evt._7)
          val version = evt._2

          // insert context into message
          msg.contextVar = ReplayContext(id, VObject(version, v_obj.obj))
          msg.thisVar = id

          v_obj = VObject(version + 1, msg.app)
        }

        (x._1, v_obj)
      }
    }

    def commit(reads: MV, writes: MV, sends: Vector[SEND]): Option[StoreFailure] = {
      val buffer = new Array[Byte](16384)

      val snapshotPlusOne = (reads ++ writes).map(x => (SerializationUtils.toBytes(x._1), x._2 + 1))

      // check whether the snapshot (versions) have been invalidated by writes (the event count must be 0)
      val checkSnapshots = snapshotPlusOne.map( rw =>
          for {
            c <- event.filter(x => x.id === rw._1 && x.event_id === rw._2).length.result
            r <- if (c == 0) DBIO.successful(); else DBIO.failed(new RuntimeException("snapshot invalid"))
          } yield r
      ).toSeq

      val insertEvents = sends.map ( s => {
        val id = s.vid.id
        val msg = s.message

        // make sure the message is cleaned from context data
        msg.thisVar = null
        msg.contextVar = null

        // prepare event record
        event += (
          toBytes(id, buffer),
          s.vid.version,
          0,
          0,
          s.level,
          0,
          toBytes(msg, buffer),
          id.toString,
          msg.typeString,
          id.typeString
        )
      })

      // both snapshot checks and inserts need to be in one transaction
      val checks_inserts = DBIO.seq(checkSnapshots ++ insertEvents: _*)

      // We need STRICT Serializability from Postgres in order to detect read/write collisions (disallow write-skew)
      val totalTransaction = checks_inserts.transactionally.withTransactionIsolation(TransactionIsolation.Serializable)

      // We block for now, we may want to return a Future or better still wait for JVM fibers
      Await.ready(db.run(totalTransaction), Duration.Inf).value match {
        case Some(x) => x match {
          case Success(_) => None
          case Failure(x) => Some(CommitFailure(x))
        }
        case None => Some(DatabaseFailure())
      }
    }
  }
}
