package net.manikin.example.bank

object ConsistencyTest {
  import net.manikin.core.context.DefaultContext.DefaultContext
  import net.manikin.core.context.store.InMemoryStore.InMemoryStore
  import net.manikin.core.context.store.slick.PostgresStore.PostgresStore
  import net.manikin.core.context.Transactor._
  import IBAN._
  import scala.util.Random
  import scala.language.implicitConversions

  val nr_accounts = 1000
  val nr_batches = 1000
  val batch_size = 10
  val initial_amount = 1000L

  def total_transfers = nr_batches * batch_size

  def main(args: Array[String]): Unit = {

    val db1 = new PostgresStore("postgres_db", 1)
    val db2 = new PostgresStore("postgres_db", 2)
    val db3 = new PostgresStore("postgres_db", 3)
    val db4 = new PostgresStore("postgres_db", 4)

    //val db1 = new InMemoryStore()

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

    var f1: Long = 0; var f2: Long =0 ; var f3: Long = 0
    // Three concurrent 'processes'
    val thread1 = new Thread { override def run() = { f1 = randomTransfers(t1, 0) } }
    val thread2 = new Thread { override def run() = { f2 = randomTransfers(t2, total_transfers * 2) } }
    val thread3 = new Thread { override def run() = { f3 = randomTransfers(t3, total_transfers * 4) } }

    thread1.start()
    thread2.start()
    thread3.start()

    thread1.join()
    thread2.join()
    thread3.join()

    case class Sum() extends Transaction[Long] {
      def eff = (0 until nr_accounts).map(a => Account.Id(IBAN("A" + a)).data.balance).sum
    }

    println("done")
    println("failures: " + (f1 + f2 + f3))

    val sum = t4.commit(TId(), Sum())
    println("sum: " + sum)
    //println("nr_events: " + db1.events.size)
    //println("max_event: " + db1.events.map(x => (x._1, x._2.size)).maxBy(_._2))
    
    assert((nr_accounts * initial_amount) == sum)  // A Bank should not lose money!
  }

  def rAmount(range: Long): Long = (Random.nextGaussian().abs * range.toDouble).round + 1
  def rAccount(nr_accounts: Int): Account.Id = Account.Id(IBAN("A" + Random.nextInt.abs % nr_accounts))

  case class RandomBatchTransfer(work: List[(Long, Account.Id, Account.Id, Long)]) extends Transaction[Unit] {
    def eff = {
      work.foreach { x =>
        val tid = Transfer.Id(x._1)
        tid ! Transfer.Create(x._2, x._3, x._4)
        tid ! Transfer.Book()
      }
    }
  }

  def randomTransfers(tx: Transactor, offset: Long): Long = {
    var tid = offset
    var failures = 0

    for (b <- 0 until nr_batches) {
      val work = (0 until batch_size).map { x =>
        var a1 = rAccount(nr_accounts)
        var a2 = rAccount(nr_accounts)
        tid += 1
        while (a1 == a2) { a1 = rAccount(nr_accounts); a2 = rAccount(nr_accounts) }
        (tid, a1, a2, rAmount(initial_amount / 50))
      }.toList

      if ((tid % 10000) == 0) println("tx: " + tx + ": " + (tid - offset))

      try tx.commit(TId(), RandomBatchTransfer(work))
      catch { case t: Throwable => /*println("t: " + t)*/  ; failures += 1 }

    }
    failures
  }
}
