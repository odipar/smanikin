package net.manikin.core.state

object StateObject {
  import net.manikin.core.TransObject._

  // A StateObject is a special kind of Object that is the product of a data Object and a simple abstract state.
  case class StateObject[+O](data: O, state: String)

  // A StateId identifies a StateObject
  trait StateId[+O] extends Id[StateObject[O]] {
    def init = StateObject(initData, "Initial")
    def initData: O

    def state(implicit ctx: Context): String = obj.state
    def old_state(implicit ctx: Context): String = old_obj.state

    def data(implicit ctx: Context): O = obj.data
    def old_data(implicit ctx: Context): O = old_obj.data

    override def typeString = {
      "StateObject("+this.init.data.getClass.getName.replace("$", ".")+",_)"
    }
  }

  // A StateObject goes through 'transitions' - from one abstract state to another
  trait StateMessage[+I <: StateId[O], O, +R] extends Message[I, StateObject[O], R] {
    def state : String = self.state
    def old_state : String = self.old_state

    def data : O = self.data
    def old_data : O = self.old_data

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
