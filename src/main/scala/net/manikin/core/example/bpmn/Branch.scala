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
      if (this != before && contains(before)) Insert(before, after) --> this
    }
    
    override def contains(other: EID)(implicit ctx: Context)  = {
      (this == other) || this().element.elems.exists(_.contains(other))
    }

    override def prettyString(level: Int)(implicit ctx: Context): String = {
      ("  " * level) + this().name +
      "{\n" +
      this().element.elems.map(x => x.prettyString(level + 1)).mkString("\n") +
      "\n" +
      ("  " * level) + "}"
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
    def pre =   elems != null && !self.contains(elem)
    def app =   element() = element().copy(elems = elems :+ elem)
    def pst =   self.contains(elem) && elems == previous_elems :+ elem
  }

  case class Insert(before: EID, after: EID) extends BranchTrs {
    def pre =   self.contains(before) && !self.contains(after)
    def app =   {
                  element() = element().copy(elems = insert(elems, before, after))
                  elems.foreach(_.insert(before, after))
                }
    def pst =   self.contains(before) && self.contains(after) && elems == insert(previous_elems, before, after)
  }
}