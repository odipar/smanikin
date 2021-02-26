package net.manikin.message

object StateObject {
  import net.manikin.core.Core._
  import net.manikin.message.ThreadLocal._

  case class SObject[+O](state: String, obj: O)

  trait SId[+O] extends Id[SObject[O]] {
    def init = SObject("Initial", ini)
    def ini: O
  }

  trait SMsg[W <: World[W], I <: SId[O], O, R] extends IMsg[W, I, SObject[O], R] {
    def nst: PartialFunction[String, String]
    def pre: Boolean
    def app: O
    def eff: R
    def pst: Boolean

    // syntax
    def old = old2.obj
    def obj = obj2.obj
    def stateObj = obj2
    def stateOld = old2
    def state = obj2.state
    def oldState = old2.state
    def obj[O2](id: Id[O2]) = obj2(id)
    def old[O2](id: Id[O2]) = old2(id)
    def obj[O2](id: SId[O2]) = obj2(id).obj
    def old[O2](id: SId[O2]) = old2(id).obj
    def state[O2](id: SId[O2]) = obj2(id).state
    def oldState[O2](id: SId[O2]) = old2(id).state

    def pre2 = { if (!nst.isDefinedAt(state)) sys.error("no next state"); else pre }
    def app2 = SObject(nst(state), app)
    def eff2 = eff
    def pst2 = { if (nst(oldState) != state) sys.error("inconsistent state transition"); else pst }
  }
}
