package net.manikin.core.example.bpmn

object OrGateway {
  import net.manikin.core.asm.AbstractStateMachine._
  import java.util.UUID
  import net.manikin.core.example.bpmn.Gateway._
  import net.manikin.core.example.bpmn.Element._

  case class OrGatewayData()

  case class OrGatewayId(uuid: UUID = UUID.randomUUID()) extends GatewayId[OrGatewayData] {
    def initGateway = OrGatewayData()

    override def traces(implicit ctx: Context): TRACES = {
      if (this().element.branches.isEmpty) Seq(Seq(this, this))
      else this().element.branches.flatMap(_.traces).map(t => Seq(this) ++ t ++ Seq(this))
    }

    override def prettyString(level: Int)(implicit ctx: Context): String = {
      ("  " * level) + this().name + "[\n" +
        this().element.branches.map(x => x.prettyString(level + 1)).mkString("\n") +
        "\n" + ("  " * level) + "]"
    }
  }
}
