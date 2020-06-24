package net.manikin.main

import scala.collection.immutable.SortedSet

object Main {
  import net.manikin.example._

  type Id = Any
  type Event = Any
  type Version = Long
  type Branch = String

  val empty = Vector[Event]()

  case class State(parents: Set[State] = Set(), reads: Map[Id, SortedSet[Version]] = Map(), writes: Map[Id, Vector[Event]] = Map()) {
    def write(id: Id, event: Event): State = {
      val events = writes.getOrElse(id, empty) :+ event
      read(id)._1.copy(writes = writes + (id -> events))
    }

    def read(id: Id): (State, Version) = {
      val read_versions = reads.getOrElse(id, SortedSet(0L))
      val latest_version = writes.getOrElse(id, empty).size
      (this.copy(reads = reads + (id -> (read_versions + latest_version))), latest_version)
    }

    def commit = State(parents = Set(this), reads, writes)

    override def toString = {
      "\nparents: " + parents.map(_.hashCode) + "\n" + "reads: " + reads + "\n" + "writes: " + writes + "\n"
    }
  }

  def main(args: Array[String]): Unit = {

    var master = State().write("A", "A0")
    master = master.write("B", "B0")
    master = master.write("C", "C0")
    master = master.commit

    var branch1 = master.read("B")._1
    branch1 = branch1.write("C", "C1")
    branch1 = branch1.write("D", "D0")
    branch1 = branch1.commit

    master = master.write("A", "A1")
    master = master.read("B")._1
    master = master.write("E", "E0")
    master = master.commit

    println("master: " + master)
    println("branch1: " + branch1)

    /*bank.SimpleTransfer.main(args)
    bank.AdvancedTransfer.main(args)
    polymorph.Main.main(args)
    memoization.Factorial.main(args)
    memoization.Fibonacci.main(args)
      */


  }
}