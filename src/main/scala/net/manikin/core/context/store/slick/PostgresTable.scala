package net.manikin.core.context.store.slick

object PostgresTable {
  import slick.jdbc.PostgresProfile.api._

  class Transaction(tag: Tag) extends Table[(Long, Long, Long, Int)](tag, "transaction") {
    def serial_id = column[Long]("serial_id", O.AutoInc)
    def tx_uuid = column[Long]("tx_uuid")
    def tx_id = column[Long]("tx_id")
    def tx_size = column[Int]("tx_size")

    def pk = primaryKey("pk_a", (tx_uuid, tx_id))
    def * = (serial_id, tx_uuid, tx_id, tx_size)
  }

  class Event(tag: Tag) extends Table[(Long, Array[Byte], Long, Long, Long, Int, Int, Array[Byte], String, String, String)](tag, "event") {
    def serial_id = column[Long]("serial_id", O.AutoInc)
    def id = column[Array[Byte]]("id")
    def event_id = column[Long]("event_id")
    def tx_uuid = column[Long]("tx_uuid")
    def tx_id = column[Long]("tx_id")
    def tx_depth = column[Int]("tx_depth")
    def tx_seq = column[Int]("tx_seq")
    def event = column[Array[Byte]]("event")
    def id_string = column[String]("id_string")
    def event_type_string = column[String]("event_type_string")
    def type_string = column[String]("type_string")

    def pk = primaryKey("pk_a", (id, event_id))
    def * = (serial_id, id, event_id, tx_uuid, tx_id, tx_depth, tx_seq, event, id_string, event_type_string, type_string)
  }

  class OrderedEvent(tag: Tag) extends Table[(Long, Array[Byte], Long, Long, Long, Int, Int, Array[Byte], String, String, String)](tag, "ordered_event") {
    def sequence_id = column[Long]("sequence_id")
    def id = column[Array[Byte]]("id")
    def event_id = column[Long]("event_id")
    def tx_uuid = column[Long]("tx_uuid")
    def tx_id = column[Long]("tx_id")
    def tx_depth = column[Int]("tx_depth")
    def tx_seq = column[Int]("tx_seq")
    def event = column[Array[Byte]]("event")
    def id_string = column[String]("id_string")
    def event_type_string = column[String]("event_type_string")
    def type_string = column[String]("type_string")

    def pk = primaryKey("pk_a", sequence_id)
    def idx = index("unique_ordered_event", (id, event_id), unique = true)
    def * = (sequence_id, id, event_id, tx_uuid, tx_id, tx_depth, tx_seq, event, id_string, event_type_string, type_string)
  }

  class CurrentSnapshot(tag: Tag) extends Table[(Long, Long, Long)](tag, "current_snapshot") {
    def sequence_id = column[Long]("sequence_id")
    def tx_uuid = column[Long]("tx_uuid")
    def tx_id = column[Long]("tx_id")

    def pk = primaryKey("pk_a", (tx_uuid, tx_id, sequence_id))
    def * = (sequence_id, tx_uuid, tx_id)
  }
  
  val transaction = TableQuery[Transaction]
  val event = TableQuery[Event]
  val current_snapshot = TableQuery[CurrentSnapshot]
  val ordered_event = TableQuery[CurrentSnapshot]
  
  /*
  create table transaction(
  	serial_id SERIAL8 not null,
    tx_uuid bigint not null,
    tx_id bigint not null,
    tx_size integer not null,
    primary key(tx_uuid, tx_id)
  );

  create table current_snapshot(
    sequence_id bigint not null,
    tx_uuid bigint not null,
    tx_id bigint not null,
    primary key(tx_uuid, tx_id, sequence_id)
  );

  create table event(
  	serial_id SERIAL8 not null,
    id bytea not null,
    event_id bigint not null,
    tx_uuid bigint not null,
    tx_id bigint not null,
    tx_depth integer not null,
    tx_seq integer not null,
    event bytea not null,
    id_string text,
    event_type_string text,
    type_string text,
    primary key(id, event_id)
  );

  create table ordered_event(
    sequence_id bigint not null,
    id bytea not null,
    event_id bigint not null,
    tx_uuid bigint not null,
    tx_id bigint not null,
    tx_depth integer not null,
    tx_seq integer not null,
    event bytea not null,
    id_string text,
    event_type_string text,
    type_string text,
    primary key(sequence_id),
    unique (id, event_id)
  );
  */
}
