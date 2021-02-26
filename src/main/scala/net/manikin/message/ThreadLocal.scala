package net.manikin.message

object ThreadLocal {
  import net.manikin.core.Core._

  private val local = new ThreadLocal[Value[_, _]]

  trait IMsg[W <: World[W], I <: Id[O], O, R] extends Message[W, I, O, R] {
    protected def pre2: Boolean
    protected def app2: O
    protected def eff2: R
    protected def pst2: Boolean

    def preCondition = id => step(id, pre2)
    def apply = id => step(id, app2)
    def effect = id => step(id, eff2)
    def postCondition= id => step(id, pst2)

    def world = get().world
    def self = get().value
    def obj2 = eval(world.obj(self))
    def old2 = eval(world.old(self))

    def obj2[O2](id: Id[O2]) = eval(world.obj(id))
    def old2[O2](id: Id[O2]) = eval(world.old(id))
    def send[I2 <: Id[O2], O2, R2](id: I2, msg: Message[W, I2, O2, R2]) = eval(world.send(id, msg))

    private def set(c: V[I]) = local.set(c)
    private def get() = { val g = local.get().asInstanceOf[V[I]] ; if (g == null) sys.error("No Local") ; else g }

    private def eval[X](f: => Value[W, X]) = { val s = self ; val r = f ; set(Value(r.world, s)) ; r.value }
    private def step[X](id: V[I], f: => X) = { try { set(id) ; val r = f ; Value(world, r) }  finally set(null) }
  }

  trait LMsg[W <: World[W], I <: Id[O], O, R] extends IMsg[W, I, O, R] {
    def pre: Boolean
    def app: O
    def eff: R
    def pst: Boolean

    // syntax
    def obj = obj2
    def old = old2
    def obj[O2](id: Id[O2]) = obj2(id)
    def old[O2](id: Id[O2]) = old2(id)

    def pre2 = pre
    def app2 = app
    def eff2 = eff
    def pst2 = pst
  }
}
