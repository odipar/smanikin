package net.manikin.core.context

object Store {
  import net.manikin.core.TransObject._
  import net.manikin.core.TransObject

  trait Store {
    def update(state: ST): ST
    def commit(reads: MV, sends: Seq[SEND]): Unit
  }

  case class ReplayContext(sid: ID, obj: VObject[_]) extends Context {
    def apply[O](id: Id[O]): VObject[O] = { if (sid == id) obj.asInstanceOf[VObject[O]] ; else error }
    def previous[O](id: Id[O]): VObject[O] = error
    def send[O, R](id: Id[O], message: Message[Id[O], O, R]): R = error
    def failure: TransObject.Failure = null
    def retries: Int = error
    def failures[O](id: Id[O]) = error
    def error = sys.error("replaying")
  }

  type SS[X] = Send[_ <: Id[X], X, Any]
  type SEND = SS[Any]

  trait Send[I <: Id[O], O, +R] {
    def level: Int
    def vid: VId[O]
    def message: Message[I, O, R]
  }

  case class ReadSend[I <: Id[O], O, +R](level: Int, vid: VId[O], message: Message[I, O, R]) extends Send[I, O, R]
  case class WriteSend[I <: Id[O], O, +R](level: Int, vid: VId[O], message: Message[I, O, R]) extends Send[I, O, R]
  case class FailureSend[I <: Id[O], O, +R](level: Int, vid: VId[O], message: Message[I, O, R]) extends Send[I, O, R]

  case class VId[+O](version: Long, serial_id: Long, id: Id[O])

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
