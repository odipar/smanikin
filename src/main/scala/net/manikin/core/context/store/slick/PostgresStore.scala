package net.manikin.core.context.store.slick

object PostgresStore {
  import slick.jdbc.PostgresProfile.api._
  import slick.jdbc.TransactionIsolation
  import scala.concurrent.Await
  import scala.concurrent.duration.Duration
  import PostgresTable._

  def main(args: Array[String]): Unit = {
    val db = Database.forConfig("manikin_db")

    try {
      for (k <- 0 until 10) {
        val insert_action = DBIO.seq(
          /*event ++= (0 until 100000).map {
            x => (Array(0), x + (k * 100000), 1, 1, 1, 1, Array(0), "", "")
          } */
          transaction ++= (0 until 100000).map {
             x => (1, x + (k * 100000), 1)
          }
        ).transactionally.withTransactionIsolation(TransactionIsolation.RepeatableRead)

        println("stm: " + event.schema.createStatements.toList.toString)

        println("insert: " + insert_action)

        Await.result(
          db.run(insert_action),
          Duration.Inf)

        println("k: " + k)
      }

    }
    finally db.close()}
}
