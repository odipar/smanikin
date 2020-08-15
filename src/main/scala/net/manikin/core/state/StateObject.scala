package net.manikin.core.state

object StateObject {
  import net.manikin.core.TransObject._

  // A StateObject is a special kind of Object that is the product of a data Object and a simple abstract state.
  case class StateObject[+O](data: O, state: String)

  // A StateId identifies a StateObject
  trait StateId[+O] extends Id[StateObject[O]] {
    def init = StateObject(initData, "Initial")
    def initData: O

    def abstractState(implicit ctx: World): String = obj.state
    def old_abstractState(implicit ctx: World): String = old_obj.state

    def state(implicit ctx: World): O = obj.data
    def old_state(implicit ctx: World): O = old_obj.data

    override def typeString = {
      "StateObject("+this.init.data.getClass.getName.replace("$", ".")+",_)"
    }
  }

  // A StateObject goes through 'transitions' - from one abstract state to another
  trait StateMessage[+I <: StateId[O], O, +R] extends Message[I, StateObject[O], R] {
    def abstractState : String = self.abstractState
    def old_abstractState : String = self.old_abstractState

    def state : O = self.state
    def old_state : O = self.old_state

    def nst: PartialFunction[String, String]
    def app = {
      val st = self.obj.state

      if (nst.isDefinedAt(st)) self.obj.copy(data = apl, state = nst(st))
      else throw FailureException(NextStateException(self, context(self), this))
    }
    def apl: O
  }

  // But such transition can go wrong
  case class NextStateException[+I <: StateId[O], O, +R](id: I, vid: VObject[StateObject[O]], message: StateMessage[I, O, R]) extends Failure
}
