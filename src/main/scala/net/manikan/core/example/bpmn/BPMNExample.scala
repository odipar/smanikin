package net.manikan.core.example.bpmn

object BPMNExample {
  import net.manikan.core.asm.AbstractStateMachine._
  import net.manikan.core.example.bpmn.Model._
  import net.manikan.core.example.bpmn.StartEvent._
  import net.manikan.core.example.bpmn.EndEvent._
  import net.manikan.core.example.bpmn.Branch.BranchId
  import net.manikan.core.example.bpmn.OrGateway.OrGatewayId
  import net.manikan.core.example.bpmn.SimpleTask.SimpleTaskId

  def main(args: Array[String]): Unit = {
    implicit val context = new Context()

    val start = StartEventId()
    val end = EndEventId()
    val model = ModelId()
    val task1 = SimpleTaskId()
    val task2 = SimpleTaskId()
    val task3 = SimpleTaskId()
    val task4 = SimpleTaskId()
    val task5 = SimpleTaskId()
    val or1 = OrGatewayId()
    val branch1 = BranchId()
    val branch2 = BranchId()
    val or2 = OrGatewayId()
    val branch3 = BranchId()
    val branch4 = BranchId()

    Init(start, end) --> model
    SetName(start, "start") --> model
    SetName(end, "end") --> model
    SetName(task1, "task1") --> model
    SetName(task2, "task2") --> model
    SetName(task3, "task3") --> model
    SetName(or1, "or1") --> model
    SetName(branch1, "branch1") --> model
    SetName(branch2, "branch2") --> model
    SetName(or2, "or2") --> model
    SetName(task4, "task4") --> model
    SetName(task5, "task5") --> model
    SetName(branch3, "branch3") --> model
    SetName(branch4, "branch4") --> model

    Insert(start, task1) --> model
    Insert(task1, or1) --> model
    Insert(or1, or2) --> model
    AddBranch(or1, branch1) --> model
    AddBranch(or1, branch2) --> model
    AddBranch(or2, branch3) --> model
    AddBranch(or2, branch4) --> model
    AddToBranch(branch1, task2) --> model
    AddToBranch(branch2, task3) --> model 
    AddToBranch(branch3, task4) --> model
    AddToBranch(branch4, task5) --> model
                                           
    
    println("state: " + context.state.map(x => (x._1, x._2)).mkString("\n"))

    println("traces: " + model.traces.mkString("\n"))

  }
}
