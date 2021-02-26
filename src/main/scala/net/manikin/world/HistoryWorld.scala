package net.manikin.world

object HistoryWorld {
  import net.manikin.core.Core._

  /* A HistoryWorld keeps history, doesn't track/version anything, but is twice as slow as SimpleWorld */
  case class HistoryWorld(prev: HistoryWorld = null, state: Map[Id[Any], Any] = Map()) extends World[HistoryWorld] {
    def old[O](id: Id[O]) = Value(this, prev.obj(id).value)
    def obj[O](id: Id[O]) = Value(this, state.getOrElse(id, id.init).asInstanceOf[O])
    def send[I <: Id[O], O, R](id: I, msg: Message[HistoryWorld, I, O, R]) = {
      val self = Value(this, id)
      val pre = msg.preCondition(self).value

      if (!pre) throw sys.error("Pre failed")
      else {
        val app = msg.apply(self).value
        val eff = msg.effect(Value(HistoryWorld(this, state + (id -> app)), id))
        val pst = Value(HistoryWorld(this, eff.world.state), id)

        if (!msg.postCondition(pst).value) throw sys.error("Post failed")
        else eff
      }
    }
  }
}
