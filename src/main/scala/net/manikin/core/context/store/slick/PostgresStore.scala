package net.manikin.core.context.store.slick

import com.twitter.chill.ScalaKryoInstantiator
import net.manikin.core.context.store.slick.PostgresTable.transaction

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
  class PostgresStore(config: String = "postgres_db", tx_uuid: Long = Random.nextLong) extends Store {
    val db = Database.forConfig(config)
    
    val kryo = {
      val i = new ScalaKryoInstantiator()
      i.setRegistrationRequired(false)
      i.newKryo()
    }
    
    def update(state: ST): ST = {
      val buffer = new Array[Byte](16384)

      state.map { x =>
        val idb = toBytes(x._1, buffer, kryo)
        var v_obj = x._2

        // replay all events that occurred after the current version of the object
        val eventQuery = event.filter(x => x.id === idb && x.event_id >= v_obj.version).result

        // We don't need to be very strict, we just don't want to see uncommitted data
        val eventQueryTrs = eventQuery.transactionally.withTransactionIsolation(TransactionIsolation.ReadCommitted)
        val events = Await.result(db.run(eventQueryTrs), Duration.Inf)

        events.foreach { evt =>
          val id = toObject[Id[Any]](evt._1, kryo)
          val msg = toObject[Message[Any, _ <: Id[Any], Any]](evt._7, kryo)
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

      val q = transaction.filter(_.tx_uuid === tx_uuid).groupBy { _ => true }.map {
        case (_, group) => group.map(_.tx_id).max
      }
      
      // Determine the next tx_id
      val max_tx_ids = Await.result(db.run(q.result), Duration.Inf)
      val tx_id = {if (max_tx_ids.isEmpty) 0L ; else max_tx_ids.head.getOrElse(0L) + 1 }
      
      val snapshotPlusOne = (reads ++ writes).map(x => (SerializationUtils.toBytes(x._1, buffer, kryo), x._2 + 1))

      // check whether the snapshot (versions) have been invalidated by writes (the event count must be 0)
      val checkSnapshots = snapshotPlusOne.map( rw =>
          for {
            c <- event.filter(x => x.id === rw._1 && x.event_id === rw._2).length.result
            r <- if (c == 0) DBIO.successful(); else DBIO.failed(new RuntimeException("snapshot invalid"))
          } yield r
      ).toSeq

      // prepare transaction record
      val transRecord = { transaction += (tx_uuid, tx_id, sends.size) }

      val insertEvents = sends.indices.map ( index => {
        val s = sends(index)
        val id = s.vid.id
        val msg = s.message

        // make sure the message is cleaned from context data
        msg.thisVar = null
        msg.contextVar = null

        // prepare event record
        event += (toBytes(id, buffer, kryo), s.vid.version, tx_uuid, tx_id, s.level, index, toBytes(msg, buffer, kryo), id.toString, msg.typeString, id.typeString)
      })

      // both snapshot checks, inserts and transaction need to be in one database Transaction
      val checks_inserts = DBIO.seq((checkSnapshots ++ insertEvents :+ transRecord): _*)

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

  def main(args: Array[String]): Unit = {

  }
}
