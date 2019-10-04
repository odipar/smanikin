package net.manikin.core.example.bpmn

object ParallelGateway {
  import net.manikin.core.asm.AbstractStateMachine._
  import java.util.UUID
  import net.manikin.core.example.bpmn.Gateway._
  import net.manikin.core.example.bpmn.Element._

  case class ParallelGatewayData()

  case class ParallelGatewayId(uuid: UUID = UUID.randomUUID()) extends GatewayId[ParallelGatewayData] {
    def initGateway = ParallelGatewayData()

    override def traces(implicit ctx: Context): TRACES = {
      product(this, this().element.branches.map(_.traces))
    }
  }
}