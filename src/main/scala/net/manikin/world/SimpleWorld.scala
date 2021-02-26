package net.manikin.world

object SimpleWorld {
  import net.manikin.core.Core._
  import net.manikin.mutable.MutableObject.MObject
  import scala.collection.mutable

  /* A SimpleWorld doesn't track/version anything and doesn't keep history but is very fast */
  case class SimpleWorld() extends MObject with World[SimpleWorld] {
    val state = mutable.Map[Id[Any], Any]()
    private var oldState: (Id[_], _) = _

    def old[O](id: Id[O]) = if (oldState._1 == id) Value(this, oldState._2.asInstanceOf[O]) ; else obj(id)
    def obj[O](id: Id[O]) = Value(this, state.getOrElse(id, id.init).asInstanceOf[O])
    def send[I <: Id[O], O, R](id: I, msg: Message[SimpleWorld, I, O, R]) = {
      val self = Value(this, id)
      val old = (id, obj(id).value)
      
      if (!msg.preCondition(self).value) throw sys.error("Pre failed")
      else {
        try {
          state.put(id, msg.apply(self).value)
          oldState = old
          val eff = msg.effect(self)
          oldState = old
          if (!msg.postCondition(self).value) throw sys.error("Post failed") ; else eff
        }
        finally { oldState = null }
      }
    }
  }
}
