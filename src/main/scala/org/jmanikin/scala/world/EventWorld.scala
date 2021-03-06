package org.jmanikin.scala.world

object EventWorld {
  import org.jmanikin.core._
  import org.jmanikin.scala.world.DefaultWorld._

  type W = EventWorld

  private class WEnv(var world: W)

  case class VObj[O](obj: O, version: Long)
  case class VId[I <: Id[O], O](id: I, version: Long)

  type STATE = Map[Id[_], VObj[_]]
  
  case class EventWorld(previous: EventWorld = null, state: STATE = Map(), actions: List[Action] = List())
    extends World[EventWorld] {

    private def read[O](id: Id[_ <: O]): VObj[O] = state.getOrElse(id, VObj(id.init, 0)).asInstanceOf[VObj[O]]

    def obj[O](id: Id[_ <: O]) = {
      val r = read(id) ; SValue(this.copy(actions = Read(id, r.version) +: actions), r.obj)
    }
    def old[O](id: Id[_ <: O]): Value[W, O] = {
      val r = previous.read(id) ; SValue(this.copy(actions = actions :+ Read(id, r.version) ), r.obj)
    }
    def send[I <: Id[O], O, E](id: I, message: Message[W, I, O, E]) = sendEnv(id, message, new WEnv(this))

    def err[E](err: String): Value[EventWorld, E] = throw new RuntimeException(err)
    
    private def sendEnv[I <: Id[O], O, E](id: I, message: Message[W, I, O, E], env: WEnv) = {
      val version = read(id).version
      val msg = message.msg(IEnv(env, id))
      env.world = this.copy(actions = List())

      if (!msg.pre.get) err[E]("Pre failed")
      else {
        val preActions = mustBeReads(env.world.actions, "Pre")

        env.world = EventWorld(this, state, List())
        val app = msg.app.get

        if (mustBeReads(env.world.actions, "Apply").exists(_.id != id)) err[E]("Apply may only read itself")

        env.world = EventWorld(this, state + (id -> VObj(app, version + 1)), List())
        val eff = msg.eff.get
        val effActions = env.world.actions

        env.world = EventWorld(this, env.world.state, List())
        if (!msg.pst.get) err[E]("Post failed")
        else {
          val pstActions = mustBeReads(env.world.actions, "Post")
          val send = Send(id, version, message, preActions, effActions, pstActions, eff)
          val next = EventWorld(this, env.world.state, actions :+ send)
          SValue(next, eff)
        }
      }
    }

    def mustBeReads(act: List[Action], step: String): List[Read[_]] = {
      if (act.exists(!_.isInstanceOf[Read[_]])) throw new RuntimeException(step + " may only read")
      else act.map(_.asInstanceOf[Read[_]])
    }
    
    def init() = EventWorld()

    private case class IEnv[I <: Id[O], O, E](env: WEnv, self: I) extends Environment[W, I, O, E] with
      DefaultBuilder[W, I, O, E] {

      def world = env.world
      def obj[O2](id: Id[_ <: O2]) = eval(world.obj(id))
      def old[O2](id: Id[_ <: O2]) = eval(world.old(id))
      def send[I2 <: Id[O2], O2, R2](id: I2, msg: Message[W, I2, O2, R2]) = eval(world.sendEnv(id, msg, env))
      private def eval[X](f: => Value[W, X]) = { val r = f; env.world = r.world; r.value }
    }
  }

  trait Action
  case class Read[O](id: Id[_ <: O], version: Long) extends Action
  case class Send[I <: Id[O], O, R](
     id: I,
     version: Long,
     message: Message[EventWorld, I, O, R],
     preActions: List[Read[_]],
     effActions: List[Action],
     pstActions: List[Read[_]],
     effResult: R)
    extends Action
}
