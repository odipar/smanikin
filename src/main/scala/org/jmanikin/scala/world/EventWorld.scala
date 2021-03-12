package org.jmanikin.scala.world

object EventWorld {
  import org.jmanikin.core._
  import org.jmanikin.scala.world.DefaultWorld._
  import org.jmanikin.test.WorldConformanceTest
  import org.jmanikin.scala.data.RAList._
  import scala.util.hashing.MurmurHash3._

  type W = EventWorld

  private class WEnv(var world: W)

  case class VObj[O](obj: O, version: Long, hash: Int)

  type STATE = Map[Id[_], VObj[_]]

  val conforms = WorldConformanceTest.check(EventWorld())
  
  case class EventWorld(previous: EventWorld = null, state: STATE = Map(), events: RAList[Event] = RAList())
    extends World[EventWorld] {

    def read[O](id: Id[_ <: O]): VObj[O] = {
      val obj = id.init
      state.getOrElse(id, VObj(obj, 0, obj.hashCode)).asInstanceOf[VObj[O]]
    }
    def obj[O](id: Id[_ <: O]) = {
      val r = read(id)
      SValue(this.copy(events = Read(id, r.version, r.hash) +: events), r.obj)
    }
    def old[O](id: Id[_ <: O]): Value[W, O] = {
      val r = previous.read(id)
      SValue(this.copy(events = Read(id, r.version, r.hash) +: events), r.obj)
    }
    def send[I <: Id[O], O, E](id: I, message: Message[I, O, E]) = sendEnv(id, message, new WEnv(this))
    def err[E](err: String): Value[EventWorld, E] = throw new RuntimeException(err)

    private def sendEnv[I <: Id[O], O, E](id: I, message: Message[I, O, E], env: WEnv) = {
      val vObj = read(id)
      val vObjHash = vObj.hash
      val msg = message.msg(IEnv(env, id))

      env.world = EventWorld(previous, state, events = RAList())

      if (!msg.pre.get) err[E]("Pre failed")
      else {
        val preActions = mustBeReads(env.world.events, "Pre")

        env.world = EventWorld(this, state, RAList())
        val app = msg.app.get

        if (mustBeReads(env.world.events, "Apply").exists(_.id != id)) err[E]("Apply may only read itself")

        val h1 = mix(mix(vObjHash, msg.hashCode), app.hashCode) // intermediate hash
        val vObj1 = VObj(app, vObj.version + 1, h1)
        env.world = EventWorld(this, state + (id -> vObj1), RAList())

        val eff = msg.eff.get
        val effActions = env.world.events.toList

        env.world = EventWorld(this, env.world.state, RAList())

        if (!msg.pst.get) err[E]("Post failed")
        else {
          val pstActions = mustBeReads(env.world.events, "Post")
          val send = Send(id, vObj.version, vObjHash, message, preActions, effActions, pstActions, eff)

          val h2 = mix(h1, send.hashCode) // final hash (mix with send)
          val vObj2 = VObj(app, vObj.version + 1, h2)
          val next = EventWorld(this, env.world.state + (id -> vObj2), send +: events)

          SValue(next, eff)
        }
      }
    }

    def rebase(other: EventWorld): EventWorld = {
      val commonPostfix = events.commonPostfix(other.events)
      val tDivergent = events.prefixList(events.size - commonPostfix)
      var nWorld = other
      tDivergent.foreach(e => nWorld = e(nWorld))
      nWorld
    }

    def merge(other: EventWorld): EventWorld = {
      val commonPostfix = events.commonPostfix(other.events)

      val tDivergent = events.prefixList(events.size - commonPostfix)
      val oDivergent = other.events.prefixList(other.events.size - commonPostfix)

      val tReads = minVIds(tDivergent.flatMap(_.minReads))
      val tWrites = minVIds(tDivergent.flatMap(_.minSends))

      val oReads = minVIds(oDivergent.flatMap(_.minReads))
      val oWrites = minVIds(oDivergent.flatMap(_.minSends))

      val r_s = intersect(tReads, oWrites)
      if (r_s.nonEmpty) sys.error("CANNOT MERGE: READ/WRITE INTERSECTION " + r_s)

      val o_t = intersect(tWrites, oReads)
      if (o_t.nonEmpty) sys.error("CANNOT MERGE: WRITE/READ INTERSECTION " + o_t)

      val s_s = intersect(tWrites, oWrites)
      if (s_s.nonEmpty) sys.error("CANNOT MERGE: WRITE/WRITE INTERSECTION " + s_s)

      val oState = other.state
      var nState = state
      var nEvents = events

      oWrites.foreach{ w =>
        val vObj = read(w.id)
        if (!(vObj.version == w.version && vObj.hash == w.hash)) sys.error("NO ALIGNMENT " + vObj + " != " + w)
      }

      oDivergent.reverse.foreach(evt => nEvents = evt +: nEvents) // append events
      oWrites.map(_.id).foreach(id => nState = nState + (id -> oState(id))) // copy state
      
      EventWorld(this, nState, nEvents)
    }

    def intersect(s1: Set[VId[_]], s2: Set[VId[_]]): Set[Id[_]] = s1.map(_.id) intersect s2.map(_.id)

    def mustBeReads(e: RAList[Event], stage: String): List[Read[_]] = {
      val evt = e.toList
      if (evt.exists(!_.isInstanceOf[Read[_]])) throw new RuntimeException(stage + " may only read")
      else evt.map(_.asInstanceOf[Read[_]])
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

  case class VId[O](id: Id[O], version: Long, hash: Int)

  trait Event {
    def prettyPrint: String
    def minReads: Set[VId[_]]
    def minSends: Set[VId[_]]
    def apply(w: EventWorld): EventWorld
  }

  case class Read[O](id: Id[_ <: O], version: Long, hash: Int) extends Event {
    def minReads = Set(VId(id, version, hash))
    def minSends = Set()
    def prettyPrint = "READ " + id + ":" + version
    def apply(w: EventWorld): EventWorld = w.obj(id).world
  }

  object Read { def apply[O](vid: VId[O]): Read[O] = Read(vid.id, vid.version, vid.hash) }

  def minVIds(seq: List[VId[_]]): Set[VId[_]] = seq.groupBy(_.id).map(_._2.minBy(_.version)).toSet

  case class Send[I <: Id[O], O, E] (
     id: I,
     version: Long,
     hash: Int,
     message: Message[I, O, E],
     preActions: List[Read[_]],
     effActions: List[Event],
     pstActions: List[Read[_]],
     effResult: E)
    extends Event {
      def minReads = {
        minVIds((preActions ++ effActions.flatMap(_.minReads).map(x => Read(x.id, x.version, x.hash)) ++ pstActions).
          map(x => VId(x.id, x.version, x.hash)))
      }
      def minSends = minVIds(VId(id, version, hash) +: effActions.flatMap(_.minSends))
      def prettyPrint = {
          "SEND " + message + " => " + id + ":" + version + "\n" +
          "  PRE:\n" + preActions.reverse.map(_.prettyPrint).mkString("\n").indent(4) +
          "  EFF: "  + effResult + "\n" + effActions.reverse.map(_.prettyPrint).mkString("").indent(4) +
          "  PST:\n" + pstActions.reverse.map(_.prettyPrint).mkString("\n").indent(4)
      }
      def apply(w: EventWorld): EventWorld = w.send(id, message).world
  }
}
