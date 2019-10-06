package net.manikin.core.example.bpmn

object Gateway {
  import net.manikin.core.asm.AbstractStateMachine._
  import net.manikin.core.example.bpmn.Element._
  import net.manikin.core.example.bpmn.Branch._

  case class GatewayData[+G](branches: Seq[BranchId] = Seq(), gateway: G)

  trait GatewayId[+G] extends ElementId[GatewayData[G]] {
    def initElement = GatewayData(Seq(), initGateway)
    def initGateway: G

    override def insert(before: EID, after: EID)(implicit ctx: Context) = {
      if (contains(before)) Insert(before, after) --> this
    }

    override def contains(other: EID)(implicit ctx: Context)  = {
      (this == other) || this().element.branches.exists(_.contains(other))
    }
  }
  
  trait GatewayTrs[+G] extends ElementTrs[GatewayData[G]] {
    def branches = self().element.branches
  }

  case class AddBranch(branch: BranchId) extends GatewayTrs[Any] {
    def pre = true
    def app = element() = element().copy(branches = branches :+ branch)
    def pst = branches == element.previous.branches :+ branch
  }

  case class Insert(before: EID, after: EID) extends GatewayTrs[Any] {
    def pre = true
    def app = element().branches.foreach(x => x.insert(before, after))
    def pst = true
  }
}

