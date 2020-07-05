package net.manikin.core.context.store.slick.postgres

object PostgresReorder {
  import PostgresTable._
  import slick.jdbc.PostgresProfile.api._

  import scala.concurrent.Await
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration.Duration
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
