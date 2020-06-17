package net.manikin.core.context.store.slick

import java.io.ByteArrayOutputStream

import net.manikin.core.TransObject
import net.manikin.core.TransObject.{Context, Failure, Id, Message, VObject}


object PostgresStore {
  import slick.jdbc.PostgresProfile.api._
  import slick.jdbc.TransactionIsolation
  import scala.concurrent.Await
  import scala.concurrent.duration.Duration
  import PostgresTable._
  import net.manikin.core.context.Store._
  import net.manikin.serialization.SerializationUtils
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.util._
  import SerializationUtils._

  class PostgresStore(config: String = "manikin_db") extends Store {
    val db = Database.forConfig(config)

    def update(state: ST): ST = {
      val baos = new ByteArrayOutputStream(1024)

      state.map{ x =>
        val idb = toBytes(x._1, baos)
        var v_obj = x._2
        var version = v_obj.version

        val eventQuery = event.filter(x => x.id === idb && x.event_id >= version).result
        val events = Await.result(db.run(eventQuery), Duration.Inf)

        events.foreach{evt =>
          val id = toObject[Id[Any]](evt._1)
          val msg = toObject[Message[Any, _ <: Id[Any] , Any]](evt._7)

          assert(evt._2 == version)
          
          msg.contextVar = ReplayContext(id, VObject(version, v_obj.obj))
          msg.thisVar = id

          v_obj = VObject(version, msg.app)
          version += 1
        }

        (x._1, v_obj)
      }
    }

    def commit(reads: MV, writes: MV, sends: Vector[SEND]): Option[StoreFailure] = {
      val baos = new ByteArrayOutputStream(1024)
      
      val snapshotPlusOne = (reads ++ writes).map(x => (SerializationUtils.toBytes(x._1), x._2 + 1))

      val checkSnapshots = snapshotPlusOne.map( rw =>
          for {
            c <- event.filter(x => x.id === rw._1 && x.event_id === rw._2).length.result
            r <- if (c == 0) DBIO.successful(); else DBIO.failed(new RuntimeException("snapshot invalid"))
          } yield r
      ).toSeq

      val insertEvents = sends.map ( s => {
        val id = s.vid.id
        val msg = s.message

        msg.thisVar = null
        msg.contextVar = null

        event += (
          toBytes(id, baos),
          s.vid.version,
          0,
          0,
          s.level,
          0,
          toBytes(msg, baos),
          id.toString,
          msg.typeString,
          id.typeString
        )
      })

      val checks_inserts = DBIO.seq(checkSnapshots ++ insertEvents: _*)
      val totalTransaction = checks_inserts.transactionally.withTransactionIsolation(TransactionIsolation.Serializable)

      // We block for now, we may want to return a Future or better still, wait for JVM18 fibers
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
