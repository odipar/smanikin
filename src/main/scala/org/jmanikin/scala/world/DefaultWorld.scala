package org.jmanikin.scala.world

object DefaultWorld {
  import org.jmanikin.core._
  import java.util.function.Supplier

  trait DefaultBuilder[I <: Id[O], O, E] extends Environment[I, O, E] with PreCondition[I, O, E]
    with Apply[I, O, E] with Effect[I, O, E] with PostCondition[I, O, E] with Msg[I, O, E] {

    type B = java.lang.Boolean

    private var _pre: Supplier[B] = _
    private var _app: Supplier[O] = _
    private var _eff: Supplier[E] = _
    private var _pst: Supplier[B] = _

    def pre(pre: Supplier[B]) = { _pre = pre; this }
    def app(app: Supplier[O]) = { _app = app; this }
    def eff(eff: Supplier[E]) = { _eff = eff; this }
    def pst(pst: Supplier[B]) = { _pst = pst; this }

    def pre = _pre
    def app = _app
    def eff = _eff
    def pst = _pst
  }

  case class SValue[W <: World[W], V](world: W, value: V) extends Value[W, V] {
    def obj[O](id: Id[_ <: O]) = world.obj(id)
    def old[O](id: Id[_ <: O]) = world.old(id)
    def send[I <: Id[O], O, E](id: I, msg: Message[I, O, E]) = world.send(id, msg)
    def init() = world.init()
  }
}
