package org.jmanikin.scala.world

import data.RAList.RAList

object EventWorld {
  import org.jmanikin.core._
  import org.jmanikin.scala.world.DefaultWorld._
  import org.jmanikin.test.WorldConformanceTest

  type W = EventWorld

  private class WEnv(var world: W)

  case class VObj[O](obj: O, version: Long)
  case class VId[I <: Id[O], O](id: I, version: Long)

  type STATE = Map[Id[_], VObj[_]]

  val conforms = WorldConformanceTest.check(EventWorld())
  
  case class EventWorld(previous: EventWorld = null, state: STATE = Map(), events: RAList[Event] = RAList())
    extends World[EventWorld] {

    { conforms }

    private def read[O](id: Id[_ <: O]): VObj[O] = state.getOrElse(id, VObj(id.init, 0)).asInstanceOf[VObj[O]]
    def obj[O](id: Id[_ <: O]) = {
      val r = read(id);
      SValue(this.copy(events = Read(id, r.version) +: events), r.obj)
    }
    def old[O](id: Id[_ <: O]): Value[W, O] = {
      val r = previous.read(id);
      SValue(this.copy(events = Read(id, r.version) +: events), r.obj)
    }
    def send[I <: Id[O], O, E](id: I, message: Message[I, O, E]) = sendEnv(id, message, new WEnv(this))

    def err[E](err: String): Value[EventWorld, E] = throw new RuntimeException(err)

    private def sendEnv[I <: Id[O], O, E](id: I, message: Message[I, O, E], env: WEnv) = {
      val version = read(id).version
      val msg = message.msg(IEnv(env, id))

      env.world = EventWorld(previous, state, events = RAList())

      if (!msg.pre.get) err[E]("Pre failed")
      else {
        val preActions = mustBeReads(env.world.events, "Pre")

        env.world = EventWorld(this, state, RAList())
        val app = msg.app.get

        if (mustBeReads(env.world.events, "Apply").exists(_.id != id)) err[E]("Apply may only read itself")

        env.world = EventWorld(this, state + (id -> VObj(app, version + 1)), RAList())
        val eff = msg.eff.get
        val effActions = env.world.events.toList

        env.world = EventWorld(this, env.world.state, RAList())

        if (!msg.pst.get) err[E]("Post failed")
        else {
          val pstActions = mustBeReads(env.world.events, "Post")
          val send = Send(id, version, message, preActions, effActions, pstActions, eff)
          val next = EventWorld(this, env.world.state, send +: events)
          SValue(next, eff)
        }
      }
    }

    def mustBeReads(a: RAList[Event], stage: String): List[Read[_]] = {
      val act = a.toList
      if (act.exists(!_.isInstanceOf[Read[_]])) throw new RuntimeException(stage + " may only read")
      else act.map(_.asInstanceOf[Read[_]])
    }

    def init() = EventWorld()

    private case class IEnv[I <: Id[O], O, E](env: WEnv, self: I) extends Environment[I, O, E] with
      DefaultBuilder[I, O, E] {

      def world = env.world
      def obj[O2](id: Id[_ <: O2]) = eval(world.obj(id))
      def old[O2](id: Id[_ <: O2]) = eval(world.old(id))
      def send[I2 <: Id[O2], O2, R2](id: I2, msg: Message[I2, O2, R2]) = eval(world.sendEnv(id, msg, env))
      private def eval[X](f: => Value[W, X]) = { val r = f; env.world = r.world; r.value }
    }
  }

  trait Event {
    def prettyPrint: String
  }
  case class Read[O](id: Id[_ <: O], version: Long) extends Event {
    def prettyPrint = "READ " + id + ":" + version
  }
  case class Send[I <: Id[O], O, E](
     id: I,
     version: Long,
     message: Message[I, O, E],
     preActions: List[Read[_]],
     effActions: List[Event],
     pstActions: List[Read[_]],
     effResult: E)
    extends Event {
      def prettyPrint = {
          "SEND " + message + " => " + id + ":" + version + "\n" +
          "  PRE:\n" + preActions.reverse.map(_.prettyPrint).mkString("\n").indent(4) +
          "  EFF: "  + effResult + "\n" + effActions.reverse.map(_.prettyPrint).mkString("").indent(4) +
          "  PST:\n" + pstActions.reverse.map(_.prettyPrint).mkString("\n").indent(4)
      }
  }
}
