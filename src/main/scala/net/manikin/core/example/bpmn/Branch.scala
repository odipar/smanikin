package net.manikin.core.example.bpmn

object Branch {
  import net.manikin.core.example.bpmn.Element._
  import net.manikin.core.asm.AbstractStateMachine._
  
  import java.util.UUID

  case class BranchData(elems: Seq[EID] = Seq())

  case class BranchId(uuid: UUID = UUID.randomUUID()) extends ElementId[BranchData] {
    def initElement = BranchData()

    override def traces(implicit ctx: Context): TRACES = {
      product(this, this().element.elems.map(x => x.traces))
    }

    override def insert(before: EID, after: EID)(implicit ctx: Context) = {
      if (contains(before)) Insert(before, after) --> this
    }
    
    override def contains(other: EID)(implicit ctx: Context)  = {
      (this == other) || this().element.elems.exists(_.contains(other))
    }
  }

  trait BranchTrs extends ElementTrs[BranchData] {
    def branch = element()
    def elems = branch.elems

    def previous_branch = element.previous
    def previous_elems = previous_branch.elems
  }

  def insert(elems: Seq[EID], before: EID, after: EID): Seq[EID] = {
    if (elems.contains(before)) elems.flatMap(x => if (x == before) Seq(x, after) ; else Seq(x))
    else elems
  }

  case class Add(elem: EID) extends BranchTrs {
    def pre = elems != null
    def app = element() = element().copy(elems = elem +: elems)
    def pst = elems == elem +: previous_elems
  }

  case class Insert(before: EID, after: EID) extends BranchTrs {
    def pre = elems != null
    def app =  {
      element() = element().copy(elems = insert(elems, before, after))
      elems.foreach(x => x.insert(before,after))
    }
    def pst = elems == insert(previous_elems, before, after)
  }
}