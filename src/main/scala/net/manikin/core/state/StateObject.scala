package net.manikin.core.state

object StateObject {
  import net.manikin.core.TransObject._

  // A StateObject is a special kind of Object that is the product of Data and a simple abstract state.
  case class StateObject[+O](data: O, state: String)

  // A StateId identifies a StateObject
  trait StateId[+O] extends Id[StateObject[O]] {
    def init = StateObject(initData, "Initial")
    def initData: O

    def state(implicit ctx: Context): String = obj.state
    def old_state(implicit ctx: Context): String = old_obj.state

    def data(implicit ctx: Context): O = obj.data
    def old_data(implicit ctx: Context): O = old_obj.data
  }

  // A StateObject goes through 'transitions' - from one abstract state to another
  trait StateMessage[+O, +I <: StateId[O], +R] extends Message[StateObject[O], I, R] {
    def state : String = self.state
    def old_state : String = self.old_state

    def data : O = self.data
    def old_data : O = self.old_data

    def nst: PartialFunction[String, String]
    def app = {
      val st = self.obj.state

      if (nst.isDefinedAt(st)) self.obj.copy(data = apl, state = nst(self.obj.state))
      else {
        val f = NextStateException(self, self.obj, this)
        throw FailureException(f)
      }
    }
    def apl: O
  }

  // But such transition can go wrong
  case class NextStateException[+O, I <: Id[O], +R](id: I, state: O, message: Message[O, I, R]) extends Failure
}
