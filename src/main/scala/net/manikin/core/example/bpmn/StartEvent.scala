package net.manikin.core.example.bpmn

object StartEvent {
  import java.util.UUID
  import net.manikin.core.example.bpmn.Element._

  case class StartEventData()
  
  case class StartEventId(uuid: UUID = UUID.randomUUID()) extends ElementId[StartEventData] {
    def initElement = StartEventData()
  }
}


