package net.manikin.core.context.store.slick.postgres

object PostgresStore {
  import PostgresTable._
  import net.manikin.core.TransObject._
  import net.manikin.core.context.Store._
  import net.manikin.serialization.SerializationUtils
  import SerializationUtils._
  import slick.jdbc.PostgresProfile.api._
  import slick.jdbc.TransactionIsolation
  import scala.concurrent.Await
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration.Duration
  import scala.language.implicitConversions
  import scala.util._
  import java.security.MessageDigest
  
  implicit val byteOrder: Ordering[Array[Byte]] = (x: Array[Byte], y: Array[Byte]) => java.util.Arrays.compare(x, y)
  
  // A Postgres backing Store
  class PostgresStore(config: String = "postgres_db", tx_uuid: Long = Random.nextLong()) extends Store {
    val db = Database.forConfig(config)
    val buffer = new Array[Byte](1024 * 1024)
    val kryo = SerializationUtils.kryoInstantiator.newKryo()
    val msgd = MessageDigest.getInstance("SHA-256")
    
    def eventQuery(idb: Rep[Array[Byte]], version: Rep[Long]) = {
      event.filter(x => x.id === idb && x.event_id >= version).sortBy(_.event_id)
    }
    val eventQueryCompiled = Compiled(eventQuery _)

    def checkSnapshot(id: Rep[Array[Byte]], event_id: Rep[Long]) = {
      event.filter(x => x.id === id && x.event_id === event_id).length
    }
    val checkSnapshotCompiled = Compiled(checkSnapshot _)

    def latestTransaction(tx_uuid: Rep[Long]) = {
      transaction.filter(_.tx_uuid === tx_uuid).groupBy { _ => true }.map {
        case (_, group) => group.map(_.tx_id).max
      }
    }
    val latestTransactionCompiled = Compiled(latestTransaction _)

    def latestSnapshot(id: Rep[Array[Byte]]) = {
      event.filter(_.id === id).groupBy{ _ => true }.map {
        case (_, group) => group.map(_.snapshot_id).max
      }
    }
    val latestSnapshotCompiled = Compiled(latestSnapshot _)
    
    def update(state: ST): ST = {

      state.map { x =>
        val id = x._1
        val idb = digest(id, buffer, kryo, msgd) // the SHA-256 hash of the serialized id
        var v_obj = x._2
        
        // lastest snapshot

        val l_snapshot = Await.result(db.run(latestSnapshotCompiled(idb).result), Duration.Inf)
        val lSnapshot = { if (l_snapshot.isEmpty) -1L ; else l_snapshot.head.getOrElse(-1L) }
        
        // replay all events that occurred after the current or latest snapshot version of the object
        val eventQuery = eventQueryCompiled(idb, v_obj.version max lSnapshot).result
        
        val events = Await.result(db.run(eventQuery), Duration.Inf)

        events.foreach { evt =>
          val msg = toObject[Message[_ <: Id[Any], Any, Any]](evt._9, kryo)
          val version = evt._3
          val serial_id = evt._1
          
          // insert context into message
          msg.msgContext = MessageContext(id, ReplayWorld(id, VObject(version, serial_id, v_obj.obj)))

          v_obj = VObject(version + 1, serial_id, msg.app)
        }

        (x._1, v_obj)
      }
    }


    def commit(reads: MV, sends: Seq[SEND]): Unit = {
      val q = latestTransactionCompiled(tx_uuid)
      
      // Determine the next tx_id
      val max_tx_ids = Await.result(db.run(q.result), Duration.Inf)
      val tx_id = {if (max_tx_ids.isEmpty) 0L ; else max_tx_ids.head.getOrElse(0L) + 1 }
      
      val snapshot = reads.map(x => (SerializationUtils.toBytes(x._1, buffer, kryo), x._2))

      // check whether the snapshot (versions) have been invalidated by writes (the event count must be 0)
      val checkSnapshots = snapshot.
        toSeq.
        sorted.      // order on ID and version to avoid expensive deadlocks
        map( rw =>
          for {
            c <- checkSnapshotCompiled(rw._1, rw._2).result
            r <- if (c == 0) DBIO.successful({}); else DBIO.failed(FailureException(SnapshotFailure()))
          } yield r
        )

      // prepare transaction record
      val transRecord = { transaction += (0, tx_uuid, tx_id, sends.size) }
      
      val prepareAndOrderEvents = sends.
        indices.
        map(index => {
          val send = sends(index)
          val id = send.vid.id
          val msg = send.message
          
          // make sure the message is cleaned from context data
          msg.msgContext = null

          OrderedMessage(digest(id, buffer, kryo, msgd), id, index, send.vid.version, send.vid.serial_id, send.level, msg)
        }).
        sortBy(x => (x.ida, x.version))  // order on ID and version to avoid expensive deadlocks

      val n_serial_id = prepareAndOrderEvents.map(x => x.serial_id).max + 1

      val insertEvents = prepareAndOrderEvents.
        map ( s => {
          // prepare event record
          event += (n_serial_id, s.ida, s.version, s.snapshotId, tx_uuid, tx_id, s.level, s.index, toBytes(s.msg, buffer, kryo) , s.id.toString, s.msg.typeString, s.id.typeString)
        })

      // both snapshot checks, inserts and transaction need to be in one database Transaction
      val checks_inserts = DBIO.seq((checkSnapshots ++ insertEvents :+ transRecord): _*)

      // We need STRICT Serializability from Postgres in order to detect read/write collisions (disallow write-skew)
      val totalTransaction = checks_inserts.transactionally.withTransactionIsolation(TransactionIsolation.Serializable)

      // We block for now, we may want to return a Future or better still wait for JVM fibers
      Await.ready(db.run(totalTransaction), Duration.Inf).value match {
        case Some(x) => x match {
          case Success(_) =>
          case Failure(x) => throw FailureException(CommitFailure(x))
        }
        case None => throw FailureException(DatabaseFailure())
      }
    }

    def tryToCreateSchema(): Unit = {
      try {
        val schema = event.schema ++ transaction.schema
        Await.result(db.run(schema.create), Duration.Inf)
      }
      catch {
        case t : Throwable =>
      }
    }
  }

  case class OrderedMessage(ida: Array[Byte], id: Id[Any], index: Int, version: Long, serial_id: Long, level: Int, msg: Message[_ <: Id[Any], Any, Any]) {
    def snapshotId = if (msg.isInstanceOf[Snapshot[_ <: Any, Any]]) version ; else -1L
  }
}
