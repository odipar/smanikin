package net.manikin.core.example.bpmn

import net.manikin.core.example.bpmn.AndGateway.AndGatewayId
import net.manikin.core.example.bpmn.Trace.TraceTrs

object BPMNExample {
  import net.manikin.core.asm.AbstractStateMachine._
  import net.manikin.core.example.bpmn.Model._
  import net.manikin.core.example.bpmn.StartEvent._
  import net.manikin.core.example.bpmn.EndEvent._
  import net.manikin.core.example.bpmn.Branch.BranchId
  import net.manikin.core.example.bpmn.OrGateway.OrGatewayId
  import net.manikin.core.example.bpmn.SimpleTask.SimpleTaskId

  def main(args: Array[String]): Unit = {
    implicit val context = new Context()

    val start = StartEventId()
    val end = EndEventId()
    val end2 = EndEventId()

    val model = ModelId()
    val task1 = SimpleTaskId()
    val task2 = SimpleTaskId()
    val task3 = SimpleTaskId()
    val task4 = SimpleTaskId()
    val task5 = SimpleTaskId()
    val task6 = SimpleTaskId()
    val or1 = OrGatewayId()
    val branch1 = BranchId()
    val branch2 = BranchId()
    val or2 = OrGatewayId()
    val branch3 = BranchId()
    val branch4 = BranchId()
    val branch5 = BranchId()

    Init(start, end) --> model
    Insert(start, task1) --> model
    Insert(task1, or1) --> model
    Insert(or1, or2) --> model
    AddOrBranch(or1, branch5) --> model
    AddOrBranch(or1, branch1) --> model
    AddOrBranch(or1, branch2) --> model
    AddToBranch(branch1, task2) --> model
    AddToBranch(branch2, task3) --> model
    AddToBranch(branch5, task6) --> model
    AddOrBranch(or2, branch3) --> model
    AddOrBranch(or2, branch4) --> model
    AddToBranch(branch3, task4) --> model
    AddToBranch(branch4, task5) --> model
    //Insert(task3, end2) --> model
    
    SetName(start, "start") --> model
    SetName(end, "end") --> model
    //SetName(end2, "end2") --> model
    SetName(task1, "task1") --> model
    SetName(task2, "task2") --> model
    SetName(task3, "task3") --> model
    SetName(or1, "or1") --> model
    SetName(branch1, "branch1") --> model
    SetName(branch2, "branch2") --> model
    SetName(or2, "or2") --> model
    SetName(task4, "task4") --> model
    SetName(task5, "task5") --> model
    SetName(task6, "task6") --> model
    SetName(branch3, "branch3") --> model
    SetName(branch4, "branch4") --> model
    SetName(branch5, "branch5") --> model

    println
    println(model.prettyStringModel(0))
    println
    println(model.prettyStringTraces(0))

    // check conformance
    val t1 = model.traces.toSet
    val t2 = model().itraces.map(x => x().element.elems).toSet

    println
    println(t1.map(x => x.map(_().name)).mkString("\n"))
    if (t1 != t2) {
      println("t1: " + t1)
      println("t2: " + t2)
    }

    println
    
    context.sent.
      filter(_.transition.isInstanceOf[TraceTrs]).
      foreach(println(_))
  }
}
