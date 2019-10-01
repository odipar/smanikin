package net.manikin.core.example.bpmn

object Model {
  import java.util.UUID
  import net.manikin.core.asm.AbstractStateMachine._
  import net.manikin.core.example.bpmn.Element._
  import net.manikin.core.example.bpmn.Branch._
  import net.manikin.core.example.bpmn.Trace._
  import net.manikin.core.example.bpmn.StartEvent._
  import net.manikin.core.example.bpmn.EndEvent._
  import net.manikin.core.example.bpmn.Gateway._

  case class ModelId  (uuid: UUID = UUID.randomUUID()) extends Id[ModelData] {
    def init = ModelData()
    def traces(implicit ctx: Context) : TRACE = this().main.traces
  }
  
  case class ModelData(main: BranchId = BranchId(), traces: Seq[TraceId] = Seq(TraceId()))

  trait ModelTrs extends Transition[ModelData] {
    def main = self().main
    def traces = self().traces
  }

  def addTraces(t: TRACE)(implicit ctx: Context): Seq[TraceId] = {
    val traces = t.map(x => (TraceId(), x)).toMap
    traces.foreach(x => Trace.Init(x._2) --> x._1)
    traces.keys.toSeq
  }
  
  case class Init(startEvent: StartEventId, endEvent: EndEventId) extends ModelTrs {
    def pre = true
    def app = {
      Element.SetName("main_branch") --> main
      Branch.Add(startEvent) --> main
      Branch.Add(endEvent) --> main
      self() = self().copy(traces = self().traces ++ addTraces(main.traces))
    }
    def pst = true
  }

  case class InitTraces(new_traces: Seq[TraceId]) extends ModelTrs {
    def pre =true
    def app = self() = self().copy(traces = new_traces)
    def pst = true
  }
  
  case class Insert(before: ElementId[Any], after: ElementId[Any]) extends ModelTrs {
    def pre = true
    def app = main.insert(before, after)
    def pst = true
  }

  case class SetName(elem: ElementId[Any], name: String) extends ModelTrs {
    def pre = true
    def app = Element.SetName(name) --> elem
    def pst = true
  }

  case class AddBranch(gateway: GatewayId[Any], branch: BranchId) extends ModelTrs {
    def pre = true
    def app = Gateway.AddBranch(branch) --> gateway
    def pst = true
  }

  case class AddToBranch(branch: BranchId, elem: ElementId[Any]) extends ModelTrs {
    def pre = true
    def app = Branch.Add(elem) --> branch
    def pst = true
  }

  case class SetTraces(new_traces: Seq[TraceId]) extends ModelTrs {
    def pre = true
    def app = self() = self().copy(traces = new_traces)
    def pst = true
  }
}

