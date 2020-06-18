package net.manikin.example.bank

import scala.util.Random

object ConsistencyTest {
  import net.manikin.core.context.DefaultContext.DefaultContext
  import net.manikin.core.context.store.slick.PostgresStore.PostgresStore
  import net.manikin.core.context.Transactor._
  import IBAN._

  val nr_accounts = 10   // High contention
  val nr_transfers = 1000
  val initial_amount = 100.0

  def main(args: Array[String]): Unit = {
    val db1 = new PostgresStore("postgres_db", 1)
    val db2 = new PostgresStore("postgres_db", 2)
    val db3 = new PostgresStore("postgres_db", 3)
    val db4 = new PostgresStore("postgres_db", 4)

    val t1 = Transactor(DefaultContext(db1))
    val t2 = Transactor(DefaultContext(db2))
    val t3 = Transactor(DefaultContext(db3))
    val t4 = Transactor(DefaultContext(db4))

    // create Accounts
    case class CreateAccounts() extends Transaction[Unit] {
      def eff = { for (a <- 0 until nr_accounts) { Account.Id(IBAN("A" + a)) ! Account.Open(initial_amount) } }
    }

    // try to commit
    try t4.commit(TId(), CreateAccounts())
    catch { case t: Throwable => println("t: " + t) }

    // Three concurrent 'processes'
    val thread1 = new Thread { override def run() { randomTransfers(t1, 0) } }
    val thread2 = new Thread { override def run() { randomTransfers(t2, nr_transfers * 1) } }
    val thread3 = new Thread { override def run() { randomTransfers(t3, nr_transfers * 2) } }

    thread1.start()
    thread2.start()
    thread3.start()

    thread1.join()
    thread2.join()
    thread3.join()     

    case class Sum() extends Transaction[Double] {
      def eff = (0 until nr_accounts).map(a => Account.Id(IBAN("A" + a)).data.balance).sum
    }
    
    val sum = t4.commit(TId(), Sum())
    println("sum: " + sum)

    assert((nr_accounts * initial_amount) == sum)  // A Bank should not lose money!
  }

  def rAmount(range: Double): Double = (Random.nextGaussian().abs * range).round
  def rAccount(nr_accounts: Int): Account.Id = Account.Id(IBAN("A" + Random.nextInt.abs % nr_accounts))

  def randomTransfers(tx: Transactor, offset: Int): Unit = {
    for (t <- 0 until nr_transfers) {
      case class RandomTransfer() extends Transaction[Unit] {
        def eff = {
          val id = Transfer.Id(t + offset)
          id ! Transfer.Create(rAccount(nr_accounts), rAccount(nr_accounts), rAmount(initial_amount / 4))
          id ! Transfer.Book()
        }
      }

      try tx.commit(TId(), RandomTransfer())
      catch { case t: Throwable => println("tx: " + t) }
    }
  }
}