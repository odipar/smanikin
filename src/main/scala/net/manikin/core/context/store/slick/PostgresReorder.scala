package net.manikin.core.context.store.slick

object PostgresReorder {
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

  class PostgresReorder(config: String = "postgres_db") {
    val db = Database.forConfig(config)
    
    def reorder(): Unit = {
      val current_sequence_id = ordered_event.map(_.sequence_id).max

      val current_snapshot_query = for {
        seq_id <- current_sequence_id.result
        snap <- current_snapshot.filter(x => x.sequence_id === seq_id).result
      } yield snap

      
      val result = Await.result(db.run(current_snapshot_query), Duration.Inf)
      val currentSnapshot = result.map(x => (x._2, x._3)).toMap


    }
  }
}
