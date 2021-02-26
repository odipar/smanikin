package net.manikin.world

object SimpleWorld {
  import net.manikin.core.Core._
  import scala.collection.immutable.HashMap
  import net.manikin.mutable.MutableObject.MObject

  /* A SimpleWorld doesn't track/version anything and doesn't keep history but is very fast */
  case class SimpleWorld() extends MObject with World[SimpleWorld] {
    var state = HashMap[Id[Any], Any]()
    var old: (Id[_], _) = _

    def old[O](id: Id[O]) = if (old._1 == id) Val(this, old._2.asInstanceOf[O]) ; else obj(id)
    def obj[O](id: Id[O]) = Val(this, state.getOrElse(id, id.init).asInstanceOf[O])
    def send[I <: Id[O], O, R](id: I, msg: Message[SimpleWorld, I, O, R]) = {
      val self = Val(this, id)
      val pre = msg.preCondition(self).value

      if (!pre) throw sys.error("Pre failed")
      else {
        val app = msg.apply(self).value
        val o = obj(id).value
        old = (id, o)
        state = state + (id -> app)
        val eff = msg.effect(self)
        old = (id, o)
        if (!msg.postCondition(self).value) throw sys.error("Post failed")
        else { old = null ; eff }
      }
    }
  }
}
