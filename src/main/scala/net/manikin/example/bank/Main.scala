package net.manikin.example.bank

import net.manikin.core.DefaultContext.DefaultContext
import net.manikin.core.InMemoryStore.InMemoryStore

object Main {
  import net.manikin.core.TransactionalObject._
  import net.manikin.core.TransactionContext._
  import Account._
  import Transaction._
  import IBAN._

  def main(args: Array[String]): Unit = {
    val s = new InMemoryStore()
    
    implicit var c1 = new DefaultContext()
    //val c2 = TransactionContext(store = s)

    val a1 = AccountId(iban = IBAN("A1"))
    val a2 = AccountId(iban = IBAN("A2"))
    val t1 = TransactionId(id = 1)
    val t2 = TransactionId(id = 2)
    
    case class T1() extends Effect {
      def eff = {
        a1 ! Open(initial = 80.0)
        a2 ! Open(initial = 120.0)
        t1 ! Create(from = a1, to = a2, amount = 30.0)
        t1 ! Commit()
        t2 ! Create(from = a1, to = a2, amount = 40.0)
        t2 ! Commit()
      }
    }

    case class T2() extends Effect {
      def eff = {
        t2 ! Create(from = a1, to = a2, amount = 40.0)
        t2 ! Commit()
      }
    }

    try {
      a1 ! Open(initial = 80.0)
      a2 ! Open(initial = 120.0)
      t1 ! Create(from = a1, to = a2, amount = 30.0)
      t1 ! Commit()
      t2 ! Create(from = a1, to = a2, amount = 100.0)
      t2 ! Commit()

      //c1.send(EId(), T1())
      //c2.send(EId(), T2())

      /*var c0 = c1
      while (c0.previousDefaultContext != null) {
        println(c0.stateMap)
        c0 = c0.previousDefaultContext
      } */

      println("a1: " + c1(a1).obj) // a1: State(Data(30.0),Opened)
      println("a2: " + c1(a2).obj) // a2: State(Data(170.0),Opened)
      println("t1: " + c1(t1).obj) // t1: State(Data(Id(IBAN(A1)),Id(IBAN(A2)),30.0),Committed)
    }
    catch {
      case _ : Throwable => {
        var c0 = c1

        while (c0.previousDefaultContext != null) {
          println(c0.writes)
          println(c0.failure)

          c0 = c0.previousDefaultContext
        }
      }
    }

    /*println("a1: " + c2(a1).obj) // a1: State(Data(30.0),Opened)
    println("a2: " + c2(a2).obj) // a1: State(Data(30.0),Opened)
    println("t2: " + c2(t2).obj) // t2: State(Data(Id(IBAN(A1)),Id(IBAN(A2)),20.0),Committed)*/


  }
}

