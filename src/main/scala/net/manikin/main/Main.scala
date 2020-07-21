package net.manikin.main

object Main {

  import scala.language.implicitConversions
  import net.manikin.example.bank.Account
  import net.manikin.example.bank.IBAN.IBAN
  import net.manikin.example.bank.Transfer
  import net.manikin.core.TransObject._
  import net.manikin.core.context.TestContext.TestContext
  import scala.collection.mutable

  def main(args: Array[String]): Unit = {
    val accounts = (1 to 3).map(a => Account.Id(IBAN("A" + a))).toArray
    val transfers = (1 to 4).map(t => Transfer.Id(t)).toArray
    val objects = accounts ++ transfers

    val msgGenerator = (msgSend(accounts, accountMsg) ++ msgSend(transfers, transferMsg(accounts))).toIndexedSeq
    println("objects: " + objects)
    println("msgGenerator.size: " + msgGenerator.size)
    
    val ctx = TestContext()

    val seen = mutable.HashSet[Map[Id[_], VObject[_]]]()
    val queue = mutable.Queue[Map[Id[_], VObject[_]]]()

    queue.enqueue(Map())

    var iters: Long = 0
    var cache: Long = 0
    var errors: Long = 0

    while(queue.nonEmpty) {
      val state = queue.dequeue
      for (i <- msgGenerator.indices) {
        try {
          ctx.withState(state)
          msgGenerator(i)(ctx)

          if (!seen.contains(ctx.state)) {
            seen.add(ctx.state)
            queue.enqueue(ctx.state)
          }
          else cache += 1
        }
        catch { case t: Throwable => errors += 1 }

        iters += 1

        if ((iters % 1000000) == 0) {
          println("#queue: " + queue.size)
          println("#iterations: " + iters)
          println("#unique states: " + seen.size)
          println("#cache: " + cache)
          println("#errors: " + errors)
          println
        }
      }
    }

    println("#objects: " + objects.size)
    println("#messages: " + msgGenerator)
    println("#iterations: " + iters)
    println("#unique states: " + seen.size)
    println("#cache: " + cache)
    println("#errors: " + errors)
  }

  def msgSend[I <: Id[O], O, R](ids: Seq[I], msg: Seq[Account.Msg]): Seq[MsgSend[I, O, R]] = {
    ids.flatMap(i => msg.map(m => MsgSend(i, m)))
  }

  case class MsgSend[I <: Id[O], O, +R](id: I, msg: Message[I, O, R]) {
    def apply(implicit c: Context) = id ! msg
  }

  def accountMsg: Seq[Account.Msg] = {
    val amounts = Seq(5, 10)
    amounts.map(amt => Account.Open(amt)) :+ Account.Close() :+ Account.ReOpen()
  }

  def transferMsg(accounts: Seq[Account.Id]): Seq[Transfer.Msg] = {
    val amounts = Array(1, 2)
    accounts.flatMap{ a1 =>
      accounts.flatMap{ a2 =>
        amounts.map{ amt =>
          Transfer.Book(a1, a2, amt)
        }
      }
    }
  }
}