package org.jmanikin.scala.event

object WorldEvent {
  import org.jmanikin.core._

  case class VersionedId[O](id: Id[O], version: Long, hash: Int)

  trait WEvent {
    def prettyPrint: String
    def minReads: Set[VersionedId[_]]
    def minSends: Set[VersionedId[_]]
    def apply[W <: World[W]](w: W): W
  }

  case class WRead[O](id: Id[_ <: O], version: Long, hash: Int) extends WEvent {
    def minReads = Set(VersionedId(id, version, hash))
    def minSends = Set()
    def prettyPrint = "READ " + id + ":" + version
    def apply[W <: World[W]](w: W): W = w.obj(id).world
  }

  def minVIds(seq: List[VersionedId[_]]): Set[VersionedId[_]] = seq.groupBy(_.id).map(_._2.minBy(_.version)).toSet

  case class WSend[I <: Id[O], O, E](
      id: I,
      version: Long,
      hash: Int,
      message: Message[I, O, E],
      preActions: List[WRead[_]],
      effActions: List[WEvent],
      pstActions: List[WRead[_]],
      effResult: E)
    extends WEvent {

    def minReads = {
      minVIds((preActions ++ effActions.flatMap(_.minReads).map(x => WRead(x.id, x.version, x.hash)) ++ pstActions).
        map(x => VersionedId(x.id, x.version, x.hash)))
    }
    def minSends = minVIds(VersionedId(id, version, hash) +: effActions.flatMap(_.minSends))
    def prettyPrint = {
      "SENT " + message + " => " + id + ":" + version + "\n" +
        "  PRE:\n" + preActions.reverse.map(_.prettyPrint).mkString("\n").indent(4) +
        "  EFF: "  + effResult + "\n" + effActions.reverse.map(_.prettyPrint).mkString("").indent(4) +
        "  PST:\n" + pstActions.reverse.map(_.prettyPrint).mkString("\n").indent(4)
    }
    def apply[W <: World[W]](w: W): W = w.send(id, message).world
  }
}
