package net.manikin.core.example.bpmn

import net.manikin.core.example.bpmn.Branch.BranchId
import net.manikin.core.example.bpmn.Gateway.GatewayId
import net.manikin.core.example.bpmn.Model.ModelData

object Trace {
  import net.manikin.core.asm.AbstractStateMachine._
  import java.util.UUID
  import net.manikin.core.example.bpmn.Element._

  case class TraceData(elems: Seq[ElementId[Any]] = Seq())

  case class TraceId(uuid: UUID = UUID.randomUUID()) extends ElementId[TraceData] {
    def initElement = TraceData()

    override def insert(before: ElementId[Any], after: ElementId[Any])(implicit ctx: Context) = {
      if (contains(before)) Insert(before, after) --> this
    }

    override def contains(other: ElementId[Any])(implicit ctx: Context)  = {
      (this == other) || this().element.elems.contains(other)
    }

    def patchElement(e: ElementId[Any], t: Seq[ElementId[Any]])(implicit ctx: Context): Unit = {
      InsertAt(this().element.elems.lastIndexOf(e), t) --> this
    }

    def cloneElement(e: ElementId[Any], t: Seq[ElementId[Any]])(implicit ctx: Context): Seq[ElementId[_]] = {
      insertAt(this().element.elems, this().element.elems.lastIndexOf(e), t)
    }

    def patchBranch(b: BranchId, t: Seq[ElementId[Any]])(implicit ctx: Context): Unit = {
      InsertAt(this().element.elems.lastIndexOf(b) - 1, t) --> this
    }

    def cloneBranch(b: BranchId, t: Seq[ElementId[Any]])(implicit ctx: Context): Seq[ElementId[_]] = {
      insertAt(this().element.elems, this().element.elems.lastIndexOf(b) - 1, t)
    }

    def patchGateway(g: GatewayId[Any], t: Seq[ElementId[Any]])(implicit ctx: Context): Unit = {
      InsertAt(this().element.elems.indexOf(g), t) --> this
    }

    def cloneGateway(g: GatewayId[Any], t: Seq[ElementId[Any]])(implicit ctx: Context): Seq[ElementId[_]] = {
      insertAt(this().element.elems, this().element.elems.indexOf(g), t)
    }
  }

  trait TraceTrs extends ElementTrs[TraceData] {
    def trace = self().element
    def elems = trace.elems

    def previous_trace = self.prev.element
    def previous_elems = previous_trace.elems
  }

  def insertAt(elems: Seq[ElementId[Any]], index: Int, elm: Seq[ElementId[Any]]): Seq[ElementId[Any]] = {
    val spl = elems.splitAt(index + 1) ; (spl._1 ++ elm) ++ spl._2
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
    def app = { InsertAt(elems.lastIndexOf(before), Seq(after)) --> self }     // assumes no duplicate tasks
    def pst = elems.contains(after)
  }
  
  case class InsertAt(index: Int, elm: Seq[ElementId[Any]]) extends TraceTrs {
    def pre = elems != null
    def app = element() = element().copy(elems = insertAt(elems, index, elm))
    def pst = elems == insertAt(previous_elems, index, elm)
  }
}
