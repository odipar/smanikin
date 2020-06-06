package net.manikin.core

object StateMachineObject {
  import net.manikin.core.TransactionalObject._

  case class State[+X](data: X, state: String)

  trait StateId[+X] extends Id[State[X]] {
    def init = State(initData, "Initial")
    def initData: X
  }

  case class DataID[+X](self: StateId[X])

  case class NextStateException[+X, I <: Id[X], +R](id: I, state: X, message: Message[X, I, R]) extends Failure

  // State transition Message
  trait StateMessage[+X, I <: StateId[X], +R] extends Message[State[X], I, R] {
    def data = DataID(self)

    def nst: PartialFunction[String, String]
    def app: R = {
      val st = self().state

      if (nst.isDefinedAt(st)) { self() = self().copy(state = nst(self().state)) ; apl }
      else {
        val f = NextStateException(self, self(), this)
        context.withFailure(f)
        throw FailureException(f)
      }
    }
    def apl: R
  }

  implicit class StateIdSyntax[+X](id: DataID[X]) {
    def apply()(implicit ctx: Context): X = id.self().data
    def prev(implicit ctx: Context): X = id.self.prev.data
  }

  implicit class StateIdSyntax2[X](id: DataID[X]) {
    def update(x: X)(implicit ctx: Context): Unit = ctx.set(id.self, id.self().copy(data = x))
  }
}
