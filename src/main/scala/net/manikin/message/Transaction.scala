package net.manikin.message

object Transaction {
  import net.manikin.core.Core._
  import StateObject._
  import java.util.UUID

  case class TID(uuid: UUID = UUID.randomUUID()) extends SId[Unit] {
    def ini = ()
  }

  trait Do[W <: World[W], R] extends SMsg[W, TID, Unit, R] {
    def nst = { _ => "Done" }
    def pre = true
    def app = ()
    def pst = true
  }
}
