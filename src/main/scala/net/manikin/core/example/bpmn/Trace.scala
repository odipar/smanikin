package net.manikin.core.example.bpmn

object Trace {

  import java.util.UUID
  import net.manikin.core.example.bpmn.Element._
  import net.manikin.core.asm.AbstractStateMachine._

  case class TraceData(elems: Seq[ElementId[Any]] = Seq())

  case class TraceId(uuid: UUID = UUID.randomUUID()) extends ElementId[TraceData] {
    def initElement = TraceData()

    override def insert(before: ElementId[Any], after: ElementId[Any])(implicit ctx: Context) = {
      if (contains(before)) Insert(before, after) --> this
    }

    override def contains(other: ElementId[Any])(implicit ctx: Context)  = {
      (this == other) || this().element.elems.exists(_.contains(other))
    }
  }

  trait TraceTrs extends ElementTrs[TraceData] {
    def trace = self().element
    def elems = trace.elems

    def previous_trace = self.prev.element
    def previous_elems = previous_trace.elems
  }

  def insert(elems: Seq[ElementId[Any]], index: Int, elm: ElementId[Any]): Seq[ElementId[Any]] = {
    val spl = elems.splitAt(index + 1) ; (spl._1 :+ elm) ++ spl._2
  }

  case class Init(new_elems: Seq[ElementId[Any]]) extends TraceTrs {
    def pre = elems != null
    def app = element() = element().copy(elems = new_elems)
    def pst = elems == new_elems
  }

  case class Add(elem: ElementId[Any]) extends TraceTrs {
    def pre = elems != null
    def app = element() = element().copy(elems = element().elems :+ elem)
    def pst = elems == previous_elems :+ elem
  }

  case class Insert(before: ElementId[Any], after: ElementId[Any]) extends TraceTrs {
    def pre = elems != null
    def app = { InsertAt(elems.lastIndexOf(before), after) --> self }     // assumes no duplicate tasks
    def pst = elems.contains(after)
  }
  
  case class InsertAt(index: Int, elm: ElementId[Any]) extends TraceTrs {
    def pre = elems != null
    def app = element() = element().copy(elems = insert(elems, index, elm))
    def pst = elems == insert(previous_elems, index, elm)
  }
}
