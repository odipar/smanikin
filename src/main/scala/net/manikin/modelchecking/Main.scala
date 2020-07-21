package net.manikin.modelchecking

object Main {

  import net.manikin.core.TransObject._
  import net.manikin.core.context.TestContext.TestContext
  import net.manikin.example.bank.IBAN.IBAN
  import net.manikin.example.bank._

  import scala.collection.mutable
  import scala.language.implicitConversions

  def main(args: Array[String]): Unit = {
    var iterations: Long = 0
    var cache: Long = 0
    var errors: Long = 0
    
    val accounts = (1 to 3).map(a => Account.Id(IBAN("A" + a)))
    val transfers = (1 to 4).map(t => Transfer.Id(t))
    val objects = accounts ++ transfers

    val msgGenerator = msgSend(accounts, accountMsg) ++ msgSend(transfers, transferMsg(accounts))
    
    val ctx = TestContext()

    val seen = mutable.HashSet[ST]()
    val queue = mutable.Queue[ST]()

    queue.enqueue(Map())

    while(queue.nonEmpty) {

      val state = queue.dequeue()

      for (i <- msgGenerator.indices) {
        try {
          ctx.withState(state)
          msgGenerator(i)(ctx)

          if (seen.contains(ctx.state)) cache += 1
          else {
            seen.add(ctx.state)
            queue.enqueue(ctx.state)
          }
        }
        catch { case t: Throwable => errors += 1 }

        iterations += 1

        if ((iterations % 1000000) == 0) {
          println("#queue: " + queue.size)
          println("#iterations: " + iterations)
          println("#unique states: " + seen.size)
          println("#cache: " + cache)
          println("#errors: " + errors)
          println()
        }
      }
    }

    println("#objects: " + objects.size)
    println("#messages: " + msgGenerator)
    println("#iterations: " + iterations)
    println("#unique states: " + seen.size)
    println("#cache: " + cache)
    println("#errors: " + errors)
  }

  def msgSend[I <: Id[O], O, R](ids: Seq[I], msg: Seq[Message[I, O, R]]): IndexedSeq[MsgSend[I, O, R]] = {
    ids.flatMap(i => msg.map(m => MsgSend[I, O, R](i, m))).toIndexedSeq
  }

  case class MsgSend[I <: Id[O], O, +R](id: I, msg: Message[I, O, R]) {
    def apply(implicit c: Context) = id ! msg
  }

  def accountMsg: Seq[Account.Msg] = {
    val amounts = Seq(5, 10)

    amounts.map(amt => Account.Open(amt)) :+
      Account.Close() :+
      Account.ReOpen()
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