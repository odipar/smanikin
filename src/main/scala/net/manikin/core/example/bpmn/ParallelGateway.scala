package net.manikin.core.example.bpmn

object ParallelGateway {
  import java.util.UUID
  import net.manikin.core.asm.AbstractStateMachine._
  import net.manikin.core.example.bpmn.Gateway._

  case class ParallelGatewayData()

  case class ParallelGatewayId(uuid: UUID = UUID.randomUUID()) extends GatewayId[ParallelGatewayData] {
    def initGateway = ParallelGatewayData()

    override def traces(implicit ctx: Context): TRACE = {
      product(this, this().element.branches.map(_.traces))
    }
  }
}