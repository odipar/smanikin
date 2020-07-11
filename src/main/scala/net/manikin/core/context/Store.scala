package net.manikin.core.context

object Store {
  import net.manikin.core.TransObject._
  import net.manikin.core.TransObject

  type ID = Id[_]
  type ST = Map[ID, VObject[_]]
  type MV = Map[ID, Long]

  trait Store {
    def update(state: ST): ST
    def commit(reads: MV, sends: Seq[SEND]): Unit
  }

  case class ReplayContext(sid: ID, obj: VObject[_]) extends Context {
    def apply[O](id: Id[O]): VObject[O] = { if (sid == id) obj.asInstanceOf[VObject[O]] ; else error }
    def previous[O](id: Id[O]): VObject[O] = error
    def send[O, I <: Id[O], R](id: I, message: Message[O, I, R]): R = error
    def failure: TransObject.Failure = null
    def retries: Int = error
    def error = sys.error("replaying")
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

  trait StoreFailure extends Failure
  case class SnapshotFailure() extends StoreFailure
  case class DatabaseFailure() extends StoreFailure
  case class CommitFailure(t: Throwable) extends StoreFailure {
    override def toString = "CommitFailure(" + t + ")"
  }

  case class CommitFailureException(f: StoreFailure) extends Exception {
    override def toString = "CommitFailureException(" + f + ")"
  }
}
