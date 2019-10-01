package net.manikan.core.example.bpmn

object Branch {
  import net.manikan.core.asm.AbstractStateMachine._
  import net.manikan.core.example.bpmn.Element._

  import java.util.UUID

  case class BranchData(elems: Seq[ElementId[Any]] = Seq())

  case class BranchId(uuid: UUID = UUID.randomUUID()) extends ElementId[BranchData] {
    def initElement = BranchData()

    override def traces(implicit ctx: Context): TRACE = {
      product(this, this().element.elems.map(x => x.traces))
    }

    override def insert(before: ElementId[Any], after: ElementId[Any])(implicit ctx: Context) = {
      if (contains(before)) Insert(before, after) --> this
    }
    
    override def contains(other: ElementId[Any])(implicit ctx: Context)  = {
      (this == other) || this().element.elems.exists(_.contains(other))
    }
  }

  trait BranchTrs extends ElementTrs[BranchData] {
    def branch = element()
    def elems = branch.elems

    def previous_branch = element.previous
    def previous_elems = previous_branch.elems
  }

  def insert(elems: Seq[ElementId[Any]], before: ElementId[Any], after: ElementId[Any]): Seq[ElementId[Any]] = {
    if (elems.contains(before)) elems.flatMap(x => if (x == before) Seq(x, after) ; else Seq(x))
    else elems
  }

  case class Add(elem: ElementId[Any]) extends BranchTrs {
    def pre = elems != null
    def app = element() = element().copy(elems = elems :+ elem)
    def pst = elems == previous_elems :+ elem
  }

  case class Insert(before: ElementId[Any], after: ElementId[Any]) extends BranchTrs {
    def pre = elems != null
    def app =  {
      element() = element().copy(elems = insert(elems, before, after))
      elems.foreach(x => x.insert(before,after))
    }
    def pst = elems == insert(previous_elems, before, after)
  }
}