package net.manikan.core.example.bpmn

object OrGateway {
  import java.util.UUID
  import net.manikan.core.asm.AbstractStateMachine._
  import net.manikan.core.example.bpmn.Gateway._

  case class OrGatewayData()

  case class OrGatewayId(uuid: UUID = UUID.randomUUID()) extends GatewayId[OrGatewayData] {
    def initGateway = OrGatewayData()

    override def traces(implicit ctx: Context): TRACE = {
      if (this().element.branches.isEmpty) Seq(Seq(this))
      else this().element.branches.flatMap(_.traces).map(t => Seq(this) ++ t ++ Seq(this))
    }
  }
}
