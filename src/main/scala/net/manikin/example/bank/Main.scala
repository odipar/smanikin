package net.manikin.example.bank


object Main {
  import net.manikin.core.TransObject._
  import net.manikin.core.Transactor._
  import net.manikin.core.DefaultContext._
  import net.manikin.core.InMemoryStore._
  
  import Account._
  import Transfer._
  import IBAN._

  def main(args: Array[String]): Unit = {
    val s = new InMemoryStore()
    
    val tx1 = Transactor(DefaultContext(store = s))
    val tx2 = Transactor(DefaultContext(store = s))

    val a1 = AccountId(iban = IBAN("A1"))
    val a2 = AccountId(iban = IBAN("A2"))
    val t1 = TransferId(id = 1)
    val t2 = TransferId(id = 2)

    case class T1() extends Transaction[Unit] {
      def eff = {
        a1 ! Open(initial = 80.0)
        a2 ! Open(initial = 120.0)
      }
    }
    
    case class T2() extends Transaction[Unit] {
      def eff = {
        t1 ! Create(from = a1, to = a2, amount = 30.0)
        t1 ! Book()
      }
    }

    case class T3() extends Transaction[Unit] {
      def eff = {
        t2 ! Create(from = a1, to = a2, amount = 40.0)
        t2 ! Book()
      }
    }

    tx1.commit(TId(), T1())
    tx1.commit(TId(), T2())
    tx2.commit(TId(), T3())
    
    println("a1: " + tx1(a1).obj) // a1: State(Data(30.0),Opened)
    println("a2: " + tx1(a2).obj) // a2: State(Data(170.0),Opened)
    println("t1: " + tx1(t1).obj) // t1: State(Data(Id(IBAN(A1)),Id(IBAN(A2)),30.0),Committed)

    println("a1: " + tx2(a1).obj) // a1: State(Data(30.0),Opened)
    println("a2: " + tx2(a2).obj) // a1: State(Data(30.0),Opened)
    println("t2: " + tx2(t2).obj) // t2: State(Data(Id(IBAN(A1)),Id(IBAN(A2)),20.0),Committed)


  }
}

