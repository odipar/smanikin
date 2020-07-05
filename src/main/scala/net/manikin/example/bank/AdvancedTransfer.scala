package net.manikin.example.bank

import net.manikin.core.context.store.slick.h2.H2Store.H2Store


object AdvancedTransfer {
  import net.manikin.core.TransObject._
  import net.manikin.core.context.Transactor._
  import net.manikin.core.context.DefaultContext._
  import net.manikin.core.context.store.InMemoryStore._
  import scala.collection.immutable.SortedSet
  import net.manikin.core.context.store.InMemoryStore
  import net.manikin.core.context.store.slick.postgres.PostgresStore.PostgresStore
  import scala.language.implicitConversions

  import IBAN._

  def main(args: Array[String]): Unit = {
    val store = new H2Store() // The Transactors share the same backing Store

    store.createSchema()
    // Two independent Contexts with associated Transactors
    val tx1 = Transactor(DefaultContext(store))
    val tx2 = Transactor(DefaultContext(store))

    // Set up identifiers
    val a1 = Account.Id(iban = IBAN("A1"))
    val a2 = Account.Id(iban = IBAN("A2"))
    val t1 = Transfer.Id(id = 1)
    val t2 = Transfer.Id(id = 2)

    // Transactions are Messages that have multiple effects
    case class T1() extends Transaction[Unit] {
      def eff = {
        a1 ! Account.Open(initial = 80)
        a2 ! Account.Open(initial = 120)
      }
    }
    
    case class T2() extends Transaction[Unit] {
      def eff = {
        t1 ! Transfer.Create(_from = a1, _to = a2, _amount = 30)
        t1 ! Transfer.Book()
      }
    }

    case class T3() extends Transaction[Unit] {
      def eff = {
        t2 ! Transfer.Create(_from = a1, _to = a2, _amount = 40)
        t2 ! Transfer.Book()
      }
    }

    tx1.commit(TId(), T1())  // tx1 is independent of tx2, but shares the same backing Store
    tx1.commit(TId(), T2())
    tx2.commit(TId(), T3())

    // All T2 dependent objects are reflected correctly (lazily) in tx2, except T1

    println("a1: " + tx2(a1).obj) // a1: StateObject(Data(10.0),Opened)
    println("a2: " + tx2(a2).obj) // a2: StateObject(Data(190.0),Opened)
    println("t1: " + tx2(t1).obj) // t1: StateObject(Transfer(null,null,0),Initial)
    println("t2: " + tx2(t2).obj) // t1: StateObject(Data(Id(IBAN(A1)),Id(IBAN(A2)),40.0),Booked)
  }
}

