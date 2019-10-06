package net.manikin.core.example.bpmn

object Trace {
  import net.manikin.core.asm.AbstractStateMachine._
  import java.util.UUID
  import net.manikin.core.example.bpmn.Element._

  case class TraceData(elems: Seq[EID] = Seq())

  case class TraceId(uuid: UUID = UUID.randomUUID()) extends ElementId[TraceData] {
    def initElement = TraceData()

    override def insert(before: EID, after: EID)(implicit ctx: Context) = {
      if (contains(before)) Insert(before, after) --> this
    }

    override def contains(other: EID)(implicit ctx: Context)  = {
      (this == other) || this().element.elems.contains(other)
    }
    
    def patchAfter(g: EID, t: Seq[EID], i: (Seq[EID], EID) => Int)(implicit ctx: Context): Unit = {
      InsertAt(i(this().element.elems, g), t) --> this
    }

    def cloneAfter(g: EID, t: Seq[EID], i: (Seq[EID], EID) => Int)(implicit ctx: Context): Seq[ElementId[_]] = {
      insertAt(this().element.elems, i(this().element.elems, g), t)
    }

    override def prettyString(level: Int)(implicit ctx: Context): String = {
      this().element.elems.map(x => x().name).mkString(" ")
    }
  }

  trait TraceTrs extends ElementTrs[TraceData] {
    def trace = self().element
    def elems = trace.elems

    def previous_trace = self.prev.element
    def previous_elems = previous_trace.elems
  }

  def insertAt(elems: Seq[EID], index: Int, elm: Seq[EID]): Seq[EID] = {
    val spl = elems.splitAt(index + 1) ; (spl._1 ++ elm) ++ spl._2
  }

  case class Init(new_elems: Seq[EID]) extends TraceTrs {
    def pre = elems != null
    def app = element() = element().copy(elems = new_elems)
    def pst = elems == new_elems
  }

  case class Add(elem: EID) extends TraceTrs {
    def pre = elems != null
    def app = element() = element().copy(elems = element().elems :+ elem)
    def pst = elems == previous_elems :+ elem
  }

  case class Insert(before: EID, after: EID) extends TraceTrs {
    def pre = elems != null
    def app = { InsertAt(elems.lastIndexOf(before), Seq(after)) --> self }     // assumes no duplicate tasks
    def pst = elems.contains(after)
  }
  
  case class InsertAt(index: Int, elm: Seq[EID]) extends TraceTrs {
    def pre = elems != null
    def app = element() = element().copy(elems = insertAt(elems, index, elm))
    def pst = elems == insertAt(previous_elems, index, elm)
  }
}
