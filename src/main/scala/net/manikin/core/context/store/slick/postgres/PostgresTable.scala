package net.manikin.core.context.store.slick.postgres

object PostgresTable {
  import slick.jdbc.PostgresProfile.api._
  import scala.language.implicitConversions

  class Transaction(tag: Tag) extends Table[(Long, Long, Long, Int)](tag, "transaction") {
    def serial_id = column[Long]("serial_id", O.AutoInc)
    def tx_uuid = column[Long]("tx_uuid")
    def tx_id = column[Long]("tx_id")
    def tx_size = column[Int]("tx_size")

    def pk = primaryKey("transaction_pk", (tx_uuid, tx_id))
    def * = (serial_id, tx_uuid, tx_id, tx_size)
  }

  class Event(tag: Tag) extends Table[(Long, Array[Byte], Long, Long, Long, Long, Int, Int, Array[Byte], String, String, String)](tag, "event") {
    def serial_id = column[Long]("serial_id")
    def id = column[Array[Byte]]("id")
    def event_id = column[Long]("event_id")
    def snapshot_id = column[Long]("snapshot_id")
    def tx_uuid = column[Long]("tx_uuid")
    def tx_id = column[Long]("tx_id")
    def tx_depth = column[Int]("tx_depth")
    def tx_seq = column[Int]("tx_seq")
    def event = column[Array[Byte]]("event")
    def id_string = column[String]("id_string")
    def event_type_string = column[String]("event_type_string")
    def type_string = column[String]("type_string")

    def pk = primaryKey("event_pk", (id, event_id))
    def snapshot_idx = index("event_snapshot_idx", (id, snapshot_id), unique = false)
    def serial_idx = index("event_serial_idx", (id, tx_uuid, tx_id), unique = false)

    def * = (serial_id, id, event_id, snapshot_id, tx_uuid, tx_id, tx_depth, tx_seq, event, id_string, event_type_string, type_string)
  }
  
  val transaction = TableQuery[Transaction]
  val event = TableQuery[Event]
}
