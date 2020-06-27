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
  import com.twitter.chill.ScalaKryoInstantiator
  import scala.language.implicitConversions
  
  implicit val byteOrder: Ordering[Array[Byte]] = (x: Array[Byte], y: Array[Byte]) => java.util.Arrays.compare(x, y)
  
  // A Postgres backing Store
  class PostgresStore(config: String = "postgres_db", tx_uuid: Long = Random.nextLong) extends Store {
    val db = Database.forConfig(config)
    
    val kryo = {
      val i = new ScalaKryoInstantiator()
      i.setRegistrationRequired(false)
      i.newKryo()
    }

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

    def update(state: ST): ST = {
      val buffer = new Array[Byte](16384)

      state.map { x =>
        val idb = toBytes(x._1, buffer, kryo)
        var v_obj = x._2

        // replay all events that occurred after the current version of the object
        val eventQuery = eventQueryCompiled(idb, v_obj.version).result
        
        val eventQueryTrs = eventQuery.transactionally.withTransactionIsolation(TransactionIsolation.RepeatableRead)
        val events = Await.result(db.run(eventQueryTrs), Duration.Inf)

        events.foreach { evt =>
          val id = toObject[Id[Any]](evt._2, kryo)
          val msg = toObject[Message[Any, _ <: Id[Any], Any]](evt._8, kryo)
          val version = evt._3

          // insert context into message
          msg.contextVar = ReplayContext(id, VObject(version, v_obj.obj))
          msg.thisVar = id

          v_obj = VObject(version + 1, msg.app)
        }

        (x._1, v_obj)
      }
    }


    def commit(reads: MV, sends: Seq[SEND]): Option[StoreFailure] = {
      val buffer = new Array[Byte](16384)

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
            r <- if (c == 0) DBIO.successful({}); else DBIO.failed(new RuntimeException("snapshot invalid"))
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
          msg.thisVar = null
          msg.contextVar = null

          OrderedMessage(toBytes(id, buffer, kryo), id, index, send.vid.version, send.level, msg)
        }).
        sortBy(x => (x.ida, x.version))  // order on ID and version to avoid expensive deadlocks


      val insertEvents = prepareAndOrderEvents.
        map ( s => {
          // prepare event record
          event += (0, s.ida, s.version, tx_uuid, tx_id, s.level, s.index, toBytes(s.msg, buffer, kryo) /*, s.id.toString, s.msg.typeString, s.id.typeString*/)
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

  case class OrderedMessage(ida: Array[Byte], id: Id[Any], index: Int, version: Long, level: Int, msg: Message[Any, _ <: Id[Any], Any])
}
