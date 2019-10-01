package net.manikan.core.example.bpmn

object SimpleTask {
  import java.util.UUID
  import net.manikan.core.example.bpmn.Element._

  case class SimpleTaskData()
  
  case class SimpleTaskId(uuid: UUID = UUID.randomUUID()) extends ElementId[SimpleTaskData] {
    def initElement = SimpleTaskData()
  }
}
