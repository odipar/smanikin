package net.manikin.core.context

object Store {
  import net.manikin.core.TransObject._

  type ID = Id[_]
  type ST = Map[ID, VObject[_]]
  type MV = Map[ID, Long]

  trait Store {
    def update(state: ST): ST
    def commit(reads: MV, writes: MV, sends: Vector[SEND]): Option[CommitFailure]
  }

  type SEND = Send[Any, _ <: Id[Any] , Any]

  trait Send[+O, I <: Id[O], +R] {
    def level: Int
    def vid: VId[O]
    def message: Message[O, I, R]
  }

  case class ReadSend[+O, I <: Id[O], +R](level: Int, vid: VId[O], message: Message[O, I, R]) extends Send[O, I, R]
  case class WriteSend[+O, I <: Id[O], +R](level: Int, vid: VId[O], message: Message[O, I, R]) extends Send[O, I, R]
  case class FailureSend[+O, I <: Id[O], +R](level: Int, vid: VId[O], message: Message[O, I, R]) extends Send[O, I, R]

  case class VId[+O](version: Long, id: Id[O])

  trait CommitFailure
  case class StoreFailure() extends CommitFailure
  case class SnapshotFailure(snapshot: MV) extends CommitFailure
  
  case class CommitFailureException(f: CommitFailure) extends Exception {
    override def toString = "CommitFailureException(" + f + ")"
  }
}
