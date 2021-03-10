package org.jmanikin.scala.world

import org.jmanikin.scala.data.RAList.RAList

object EventWorld {
  import org.jmanikin.core._
  import org.jmanikin.scala.world.DefaultWorld._
  import org.jmanikin.test.WorldConformanceTest

  type W = EventWorld

  private class WEnv(var world: W)

  case class VObj[O](obj: O, version: Long)

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

    def merge(other: EventWorld): EventWorld = {
      val commonPostfix = events.commonPostfix(other.events)

      val tDivergent = events.prefixList(events.size - commonPostfix)
      val oDivergent = other.events.prefixList(other.events.size - commonPostfix)

      val thisReads = minVIds(tDivergent.flatMap(_.minReads))
      val thisWrites = minVIds(tDivergent.flatMap(_.minSends))

      val otherReads = minVIds(oDivergent.flatMap(_.minReads))
      val otherWrites = minVIds(oDivergent.flatMap(_.minSends))

      println("thisReads: " + thisReads)
      println("thisWrites: " + thisWrites)

      println("otherReads: " + otherReads)
      println("otherWrites: " + otherWrites)

      val r_s = intersect(thisReads, otherWrites)
      if (r_s.nonEmpty) sys.error("CANNOT MERGE: READ/WRITE INTERSECTION " + r_s)

      val o_t = intersect(thisWrites, otherReads)
      if (o_t.nonEmpty) sys.error("CANNOT MERGE: WRITE/READ INTERSECTION " + o_t)

      val s_s = intersect(thisWrites, otherWrites)
      if (s_s.nonEmpty) sys.error("CANNOT MERGE: WRITE/WRITE INTERSECTION " + s_s)

      val other_state = other.state
      var new_state = state
      var new_events = events

      oDivergent.reverse.foreach(evt => new_events = evt +: new_events) // append events
      otherWrites.map(_.id).foreach(id => new_state = new_state + (id -> other_state(id))) // copy state

      EventWorld(this, new_state, new_events)
    }

    def intersect(s1: Set[VId[_]], s2: Set[VId[_]]): Set[Id[_]] = s1.map(_.id) intersect s2.map(_.id)

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

  case class VId[O](id: Id[O], version: Long)

  trait Event {
    def prettyPrint: String
    def minReads: Set[VId[_]]
    def minSends: Set[VId[_]]
  }

  case class Read[O](id: Id[_ <: O], version: Long) extends Event {
    def prettyPrint = "READ " + id + ":" + version
    def minReads = Set(VId(id, version))
    def minSends = Set()
  }

  object Read { def apply[O](vid: VId[O]): Read[O] = Read(vid.id, vid.version) }

  def minVIds(seq: List[VId[_]]): Set[VId[_]] = seq.groupBy(_.id).map(_._2.minBy(_.version)).toSet

  case class Send[I <: Id[O], O, E] (
     id: I,
     version: Long,
     message: Message[I, O, E],
     preActions: List[Read[_]],
     effActions: List[Event],
     pstActions: List[Read[_]],
     effResult: E)
    extends Event {
      def minReads = {
        minVIds((preActions ++ effActions.flatMap(_.minReads).map(x => Read(x.id, x.version)) ++ pstActions).
          map(x => VId(x.id, x.version)))
      }
      def minSends = minVIds(VId(id, version) +: effActions.flatMap(_.minSends))
      def prettyPrint = {
          "SEND " + message + " => " + id + ":" + version + "\n" +
          "  PRE:\n" + preActions.reverse.map(_.prettyPrint).mkString("\n").indent(4) +
          "  EFF: "  + effResult + "\n" + effActions.reverse.map(_.prettyPrint).mkString("").indent(4) +
          "  PST:\n" + pstActions.reverse.map(_.prettyPrint).mkString("\n").indent(4)
      }
  }
}
