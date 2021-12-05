package org.jmanikin.scala.world

object EventWorld {
  import org.jmanikin.core._
  import org.jmanikin.scala.world.DefaultWorld._
  import org.jmanikin.test.WorldConformanceTest
  import org.jmanikin.scala.data.RAList._
  import org.jmanikin.scala.event.WorldEvent._
  import scala.util.hashing.MurmurHash3._

  type W = EventWorld

  private class WEnv(var world: W)

  case class VersionedObj[O](obj: O, version: Long, hash: Int)

  type STATE = Map[Id[_], VersionedObj[_]]

  val conforms = WorldConformanceTest.check(EventWorld())
  
  case class EventWorld(previous: EventWorld = null, state: STATE = Map(), events: RAList[WEvent] = RAList())
    extends World[EventWorld] {

    def read[O](id: Id[_ <: O]): VersionedObj[O] = {
      val obj = id.init
      state.getOrElse(id, VersionedObj(obj, 0, obj.hashCode)).asInstanceOf[VersionedObj[O]]
    }
    def obj[O](id: Id[_ <: O]) = {
      val r = read(id)
      SValue(this.copy(events = WRead(id, r.version, r.hash) +: events), r.obj)
    }
    def old[O](id: Id[_ <: O]): Value[W, O] = {
      val r = previous.read(id)
      SValue(this.copy(events = WRead(id, r.version, r.hash) +: events), r.obj)
    }
    def send[I <: Id[O], O, E](id: I, message: Message[I, O, E]) = {
      sendEnv(id, message, new WEnv(this))
    }
    def err[E](err: String): Value[EventWorld, E] = throw new RuntimeException(err)

    private def sendEnv[I <: Id[O], O, E](id: I, message: Message[I, O, E], env: WEnv) = {
      val vObj = read(id)
      val vObjHash = vObj.hash
      val msg = message.msg(IEnv(env, id))

      env.world = EventWorld(previous, state, events = RAList())

      if (!msg.pre.get) preErr(id, vObj, message)
      else {
        val preActions = checkReads(env.world.events, id, vObj, message, "Pre")

        env.world = EventWorld(this, state, RAList())
        val app = msg.app.get

        if (checkReads(env.world.events, id, vObj, message, "Apply").exists(_.id != id)) {
          appErr[I, O, E](id, vObj, message)
        }

        val h1 = mix(mix(vObjHash, msg.hashCode), app.hashCode) // intermediate hash
        env.world = EventWorld(this, state + (id -> VersionedObj(app, vObj.version + 1, h1)), RAList())

        val eff = msg.eff.get
        val effActions = env.world.events.toList

        env.world = EventWorld(this, env.world.state, RAList())

        if (!msg.pst.get) pstErr[I, O, E](id, vObj, message)
        else {
          val pstActions = checkReads(env.world.events, id, vObj, message, "Post")
          val send = WSend(id, vObj.version, vObjHash, message, preActions, effActions, pstActions, eff)
          val next = EventWorld(this, env.world.state, send +: events)

          SValue(next, eff)
        }
      }
    }

    override def toString = events.reverseIterator.map(_.prettyPrint).mkString("\n")
    
    def rebase(other: EventWorld): EventWorld = {
      var nWorld = other
      events.prefixList(events.size - events.commonPostfix(other.events)).foreach(e => nWorld = e(nWorld))
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

      checkIntersection("WRITE/WRITE", tWrites, oWrites)
      checkIntersection("READ/WRITE", tReads, oWrites)
      checkIntersection("WRITE/READ", tWrites, oReads)

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

    def checkIntersection(err: String, s1: Set[VersionedId[_]], s2: Set[VersionedId[_]]): Unit = {
      val r = s1.map(_.id) intersect s2.map(_.id)
      if (r.nonEmpty) sys.error("CANNOT MERGE: " + err + " INTERSECTION " + r)
    }

    def preErr[I <: Id[O], O, E](id: I, vObj: VersionedObj[O], msg: Message[I, O, E]) = throw PreErr(id, vObj, msg)
    def appErr[I <: Id[O], O, E](id: I, vObj: VersionedObj[O], msg: Message[I, O, E]) = throw AppErr(id, vObj, msg)
    def pstErr[I <: Id[O], O, E](id: I, vObj: VersionedObj[O], msg: Message[I, O, E]) = throw PstErr(id, vObj, msg)

    def checkReads[I <: Id[O], O, E]
      (e: RAList[WEvent], id: I, vObj: VersionedObj[O], msg: Message[I, O, E], stage: String): List[WRead[_]] = {
        val evt = e.toList
        if (evt.exists(!_.isInstanceOf[WRead[_]])) throw ReadErr(id, vObj, msg)
        else evt.map(_.asInstanceOf[WRead[_]])
    }

    def init() = EventWorld()

    private case class IEnv[I <: Id[O], O, E](env: WEnv, self: I) extends Environment[I, O, E] with
      DefaultBuilder[I, O, E] {
      
      def world = env.world
      def obj[O2](id: Id[_ <: O2]) = eval(world.obj(id))
      def old[O2](id: Id[_ <: O2]) = eval(world.old(id))
      def send[I2 <: Id[O2], O2, R2](id: I2, msg: Message[I2, O2, R2]) = {
        val snapshot = env.world

        try { eval(world.sendEnv(id, msg, env)) }
        catch { case e: Throwable => env.world = snapshot ; throw e }
      }
      private def eval[X](f: => Value[W, X]) = { val r = f; env.world = r.world; r.value }
    }
  }

  trait WorldErr extends Exception
  
  trait MsgErr[I <: Id[O], O, E] extends WorldErr {
    def id: I
    def vObj: VersionedObj[O]
    def msg: Message[I, O, E]
    override def toString: String = getClass.getSimpleName + "(" + id + "," + vObj + "," + msg + ")"
  }

  case class ReadErr[I <: Id[O], O, E](id: I, vObj: VersionedObj[O], msg: Message[I, O, E]) extends MsgErr[I, O, E]
  case class PreErr[I <: Id[O], O, E](id: I, vObj: VersionedObj[O], msg: Message[I, O, E]) extends MsgErr[I, O, E]
  case class AppErr[I <: Id[O], O, E](id: I, vObj: VersionedObj[O], msg: Message[I, O, E]) extends MsgErr[I, O, E]
  case class PstErr[I <: Id[O], O, E](id: I, vObj: VersionedObj[O], msg: Message[I, O, E]) extends MsgErr[I, O, E]
}
