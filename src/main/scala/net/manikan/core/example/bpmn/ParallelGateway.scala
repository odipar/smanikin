package net.manikan.core.example.bpmn

object ParallelGateway {
  import java.util.UUID
  import net.manikan.core.asm.AbstractStateMachine._
  import net.manikan.core.example.bpmn.Gateway._
  
  case class ParallelGatewayData()

  case class ParallelGatewayId(uuid: UUID = UUID.randomUUID()) extends GatewayId[ParallelGatewayData] {
    def initGateway = ParallelGatewayData()

    override def traces(implicit ctx: Context): TRACE = {
      product(this, this().element.branches.map(_.traces))
    }
  }
}