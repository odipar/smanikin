package net.manikin.main


object Main {

  import scala.language.implicitConversions
  import net.manikin.example.bank.Account
  import net.manikin.example.bank.IBAN.IBAN
  import net.manikin.example.bank.Transfer
  import scala.util.Random
  import net.manikin.core.TransObject._
  import net.manikin.core.context.TestContext.TestContext
  import scala.collection.mutable

  def main(args: Array[String]): Unit = {
    val accounts = (1 to 3).map(a => Account.Id(IBAN("A" + a))).toArray
    val transfers = (1 to 4).map(t => Transfer.Id(t)).toArray

    val generator = BigConcat(accountEventGenerator(accounts, accountMsg), transferEventGenerator(transfers, transferMsg(accounts)))

    val ctx = TestContext()

    val seen = mutable.HashSet[Map[Id[_], VObject[_]]]()
    val queue = mutable.Queue[Map[Id[_], VObject[_]]]()

    seen.add(Map())
    queue.enqueue(Map())

    var iters: Long = 0
    var cache: Long = 0

    while(queue.nonEmpty) {
      val state = queue.dequeue
      for (i <- 0L until generator.length) {
        try {
          ctx.withState(state)
          generator(i)(ctx)

          if (!seen.contains(ctx.state)) {
            seen.add(ctx.state)
            queue.enqueue(ctx.state)
          }
          else cache += 1
        }
        catch {case t: Throwable => }

        iters += 1

        if ((iters % 1000000) == 0) {
          println("#queue: " + queue.size)
          println("#iterations: " + iters)
          println("#unique states: " + seen.size)
          println("#cache: " + cache)
          println
        }
      }
    }

    println("#objects: " + (accounts.size + transfers.size))
    println("#messages: " + generator.length)
    println("#iterations: " + iters)
    println("#unique states: " + seen.size)

  }

  def select[X](s: Array[X]): X = s(Random.between(0, s.length))

  def accountEventGenerator(accounts: IndexedSeq[Account.Id], msg: BigSeq[Account.Msg]) = {
    val ac = BigProduct(BigIndexedSeq(accounts), msg)

    BigMap(ac){x => MsgSend(x._1, x._2) }
  }

  def transferEventGenerator(transfers: IndexedSeq[Transfer.Id], msg: BigSeq[Transfer.Msg]) = {
    val tr = BigProduct(BigIndexedSeq(transfers), msg)
    
    BigMap(tr){x => MsgSend(x._1, x._2) }
  }

  case class MsgSend[I <: Id[O], O, +R](id: I, msg: Message[I, O, R]) {
    def apply(implicit c: Context) = id ! msg
  }

  def accountMsg: BigSeq[Account.Msg] = {
    val amounts = Array(5, 10)

    val amt = BigArraySeq(amounts)
    val open = BigMap(amt)(Account.Open(_))
    val close_reopen = BigArraySeq(Array(Account.Close(), Account.ReOpen()))

    BigConcat(open, close_reopen)
  }


  def transferMsg(accounts: IndexedSeq[Account.Id]): BigSeq[Transfer.Msg] = {
    val amounts = Array(1, 2)

    val a1 = BigIndexedSeq(accounts)
    val a2 = a1
    val i2 = BigArraySeq(amounts)

    val p1 = BigProduct(a1, a2)
    val p2 = BigProduct(p1, i2)

    BigMap(p2)(x => Transfer.Book(x._1._1, x._1._2, x._2))
  }

  trait BigSeq[+X] {
    def length: Long
    def apply(i: Long): X
  }

  case class BigElementSeq[+X](x: X) extends BigSeq[X] {
    def length = 1
    def apply(i: Long) = { if (i != 0) x ; else sys.error("index out of bounds") }
  }

  case class BigArraySeq[X](x: Array[X]) extends BigSeq[X] {
    val length = x.length
    def apply(i: Long) = { if (i >= 0 || i < length) x(i.toInt) ; else sys.error("index out of bounds") }
  }

  case class BigIndexedSeq[+X](x: IndexedSeq[X]) extends BigSeq[X] {
    val length = x.length
    def apply(i: Long) = { if (i >= 0 || i < length) x(i.toInt) ; else sys.error("index out of bounds") }
  }

  case class BigProduct[+X, +Y](x: BigSeq[X], y: BigSeq[Y]) extends BigSeq[(X, Y)] {
    val length = x.length * y.length
    def apply(i: Long) = (x(i % x.length), y(i / x.length))
  }

  case class BigMap[X, +Y](x: BigSeq[X])(f: X => Y) extends BigSeq[Y] {
    val length = x.length
    def apply(i: Long) = f(x(i))
  }
  
  case class BigConcat[+X](x1: BigSeq[X], x2: BigSeq[X]) extends BigSeq[X] {
    val length = x1.length + x2.length
    def apply(i: Long) = {
      if (i >= x1.length) x2(i - x1.length)
      else x1(i)
    }
  }
}