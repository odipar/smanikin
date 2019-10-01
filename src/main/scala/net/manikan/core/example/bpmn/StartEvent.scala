package net.manikan.core.example.bpmn

object StartEvent {
  import java.util.UUID
  import net.manikan.core.example.bpmn.Element._

  case class StartEventData()
  
  case class StartEventId(uuid: UUID = UUID.randomUUID()) extends ElementId[StartEventData] {
    def initElement = StartEventData()
  }
}


