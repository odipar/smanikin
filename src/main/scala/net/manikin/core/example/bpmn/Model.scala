package net.manikin.core.example.bpmn

object Model {
  import net.manikin.core.asm.AbstractStateMachine._
  import java.util.UUID
  import net.manikin.core.example.bpmn.Element._
  import net.manikin.core.example.bpmn.Branch._
  import net.manikin.core.example.bpmn.Trace._
  import net.manikin.core.example.bpmn.StartEvent._
  import net.manikin.core.example.bpmn.EndEvent._
  import net.manikin.core.example.bpmn.Gateway._

  case class ModelId  (uuid: UUID = UUID.randomUUID()) extends Id[ModelData] {
    def init = ModelData()
    def traces(implicit ctx: Context) : TRACES = this().main.traces
  }
  
  case class ModelData(main: BranchId = BranchId(), traces: Seq[TraceId] = Seq())

  trait ModelTrs extends DefaultTrs[ModelData] {
    def main = self().main
    def traces = self().traces
  }

  def addTraces(s: Id[ModelData], t: TRACES)(implicit ctx: Context): Unit = {
    val traces = t.map(x => (TraceId(), x)).toMap
    traces.foreach(x => Trace.Init(x._2) --> x._1)
    s() = s().copy(traces = s().traces ++ traces.keys.toSeq)
  }
  
  case class Init(startEvent: StartEventId, endEvent: EndEventId) extends ModelTrs {
    def pre = true
    def app = {
      Element.SetName("main_branch") --> main
      Branch.Add(startEvent) --> main
      Branch.Add(endEvent) --> main
      addTraces(self, main.traces)
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
    def app = {
      main.insert(before, after)

      val sub_traces = after.traces
      val target_traces = self().traces.filter(_.contains(before))

      target_traces.foreach { t =>
        val new_traces = sub_traces.tail.map(st => t.cloneElement(before, st))
        addTraces(self, new_traces)
        t.patchElement(before, sub_traces.head)
      }
    }
    def pst = true
  }

  case class SetName(elem: ElementId[Any], name: String) extends ModelTrs {
    def pre = true
    def app = Element.SetName(name) --> elem
    def pst = true
  }

  case class AddBranch(gateway: GatewayId[Any], branch: BranchId) extends ModelTrs {
    def pre = true
    def app = {
      if (gateway().element.branches.nonEmpty) {

        Gateway.AddBranch(branch) --> gateway

        val tt = self().main.traces.filter(_.contains(gateway))
        addTraces(self, tt)
      }
      else {
        Gateway.AddBranch(branch) --> gateway

        val sub_traces = branch.traces
        val target_traces = self().traces.filter(_.contains(gateway))

        target_traces.foreach { t =>
          val new_traces = sub_traces.tail.map(st => t.cloneGateway(gateway, st))
          addTraces(self, new_traces)

          t.patchGateway(gateway, sub_traces.head)
        }
      }
    }

    def pst = true
  }

  case class AddToBranch(branch: BranchId, elem: ElementId[Any]) extends ModelTrs {
    def pre = true
    def app = {
      Branch.Add(elem) --> branch

      val sub_traces = elem.traces
      val target_traces = self().traces.filter(_.contains(branch))
      
      target_traces.foreach { t =>
        val new_traces = sub_traces.tail.map(st => t.cloneBranch(branch, st))
        addTraces(self, new_traces)
        
        t.patchBranch(branch, sub_traces.head)
      }
    }
    def pst = true
  }
}

