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
  
  case class Insert(before: EID, after: EID) extends ModelTrs {
    def pre = true
    def app = {
      main.insert(before, after)
      cloneAndPatch(self, before, after, lastIndex)
    }
    def pst = true
  }

  case class SetName(elem: EID, name: String) extends ModelTrs {
    def pre = true
    def app = Element.SetName(name) --> elem
    def pst = true
  }

  case class AddBranch(gateway: GatewayId[Any], branch: BranchId) extends ModelTrs {
    def pre = true
    def app = {
      if (gateway().element.branches.nonEmpty) {

        Gateway.AddBranch(branch) --> gateway

        val tt = self().main.traces.filter(_.contains(branch))
        addTraces(self, tt)
      }
      else {
        Gateway.AddBranch(branch) --> gateway
        cloneAndPatch(self, gateway, branch, firstIndex)
      }
    }

    def pst = true
  }

  case class AddToBranch(branch: BranchId, elem: EID) extends ModelTrs {
    def pre = true
    def app = {
      Branch.Add(elem) --> branch
      cloneAndPatch(self, branch, elem, firstIndex)
    }
    def pst = true
  }

  def firstIndex(elems: Seq[EID], e: EID): Int = elems.indexOf(e)
  def lastIndex(elems: Seq[EID], e: EID): Int = elems.lastIndexOf(e)

  def cloneAndPatch(self: Id[ModelData], target: EID, new_element: EID, i: (Seq[EID], EID) => Int)(implicit ctx: Context) = {
    val sub_traces = new_element.traces
    val target_traces = self().traces.filter(_.contains(target))

    target_traces.foreach { t =>
      val new_traces = sub_traces.tail.map(st => t.cloneAfter(target, st, i))
      addTraces(self, new_traces)

      t.patchAfter(target, sub_traces.head, i)
    }
  }
}

