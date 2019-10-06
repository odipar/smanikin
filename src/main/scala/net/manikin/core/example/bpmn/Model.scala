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

  case class ModelId(uuid: UUID = UUID.randomUUID()) extends Id[ModelData] {
    def init = ModelData()
    def traces(implicit ctx: Context): TRACES = this().main.traces
    def contains(elm: EID)(implicit ctx: Context): Boolean = this().main.contains(elm)
    def prettyStringModel(level: Int)(implicit ctx: Context): String = this().main.prettyString(level)
    def prettyStringTraces(level: Int)(implicit ctx: Context): String = {
      val tr = this().traces
      tr.indices.map(i => { val x = tr(i) ; (i + 1).toString + ": " + x.prettyString(level + 1)}).mkString("\n")
    }
  }
  
  case class ModelData(main: BranchId = BranchId(), traces: Seq[TraceId] = Seq())

  trait ModelTrs extends Transition[ModelData] {
    type ID = ModelId

    def main = self().main
    def traces = self().traces
  }

  def addTraces(self: ModelId, t: TRACES)(implicit ctx: Context): Unit = {
    val traces = t.map(x => (TraceId(), x)).toMap
    traces.foreach(x => Trace.Init(x._2) --> x._1)
    self() = self().copy(traces = self().traces ++ traces.keys.toSeq)
  }
  
  case class Init(startEvent: StartEventId, endEvent: EndEventId) extends ModelTrs {
    def pre = !self.contains(startEvent) && !self.contains(endEvent) && self().traces.isEmpty
    def app = {
                Element.SetName("model") --> main
                addTraces(self, main.traces)
                AddToBranch(main, startEvent) --> self
                AddToBranch(main, endEvent) --> self
              }
    def pst = self.contains(startEvent) && self.contains(endEvent) && self().traces.size == 1
  }
  
  case class Insert(before: EID, after: EID) extends ModelTrs {
    def pre =   self.contains(before) && !self.contains(after)
    def app =   { main.insert(before, after) ; cloneAndPatch(self, before, after, afterLastIndex) }
    def pst =   self.contains(before) && self.contains(after)
  }

  case class SetName(elem: EID, name: String) extends ModelTrs {
    def pre =   true
    def app =   Element.SetName(name) --> elem
    def pst =   true
  }

  case class AddBranch(gateway: GatewayId[Any], branch: BranchId) extends ModelTrs {
    def pre =   self.contains(gateway) && !self.contains(branch)
    def app =   {
                  if (gateway().element.branches.nonEmpty) {
                    Gateway.AddBranch(branch) --> gateway
                    addTraces(self, self.traces.filter(_.contains(branch)))
                  }
                  else {
                    Gateway.AddBranch(branch) --> gateway
                    cloneAndPatch(self, gateway, branch, afterFirstIndex)
                  }
                }
    def pst =   self.contains(gateway) && self.contains(branch)
  }

  case class AddToBranch(branch: BranchId, elem: EID) extends ModelTrs {
    def pre =   self.contains(branch) && !self.contains(elem)
    def app =   { Branch.Add(elem) --> branch ; cloneAndPatch(self, branch, elem, beforeLastIndex) }
    def pst =   self.contains(branch) && self.contains(elem)
  }

  def beforeFirstIndex(elems: Seq[EID], e: EID): Int = elems.indexOf(e) - 1
  def afterFirstIndex(elems: Seq[EID], e: EID): Int = elems.indexOf(e)
  def beforeLastIndex(elems: Seq[EID], e: EID): Int = elems.lastIndexOf(e) - 1
  def afterLastIndex(elems: Seq[EID], e: EID): Int = elems.lastIndexOf(e)

  def cloneAndPatch(self: ModelId, target: EID, elem: EID, i: (Seq[EID], EID) => Int)(implicit ctx: Context) = {
    val sub_traces = elem.traces
    self().traces.filter(_.contains(target)).foreach { t =>
      addTraces(self, sub_traces.tail.map(st => t.clone(target, st, i)))
      t.patch(target, sub_traces.head, i)
    }
  }
}

