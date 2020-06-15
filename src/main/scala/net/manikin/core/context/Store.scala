package net.manikin.core.context

object Store {
  import net.manikin.core.TransObject._

  trait Store {
    def update(state: Map[Id[_], VObject[_]]): Map[Id[_], VObject[_]]
    def commit(reads: Map[Id[_], Long], writes: Map[Id[_], Long], sends: Vector[STYPE]): Option[CommitFailure]
  }
  
  case class VId[+O](version: Long, id: Id[O])
  type STYPE = Send[Any, _ <: Id[Any] , Any]

  trait Send[+O, I <: Id[O], +R] {
    def level: Int
    def vid: VId[O]
    def message: Message[O, I, R]
  }

  trait CommitFailure
  case class StoreFailure() extends CommitFailure
  case class SnapshotFailure(snapshot: Map[Id[_], Long]) extends CommitFailure

  case class WriteFailureException(f: CommitFailure) extends Exception {
    override def toString = "WriteFailureException(" + f + ")"
  }
}
