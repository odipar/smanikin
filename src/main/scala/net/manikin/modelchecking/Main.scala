package net.manikin.modelchecking

import net.manikin.serialization.SerializationUtils

object Main {
  import net.manikin.core.TransObject._
  import net.manikin.core.context.TestContext.TestContext
  import net.manikin.example.bank._
  import net.manikin.example.bank.IBAN.IBAN

  import scala.collection.mutable
  import scala.language.implicitConversions

  // Simple model checker, without trace information
  // TODO: create DSL
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

    SerializationUtils.time(
      while(queue.nonEmpty) {

        val state = queue.dequeue()

        for (i <- msgGenerator.indices) {
          try {
            msgGenerator(i)(ctx.withState(state))

            if (seen.contains(ctx.state)) cache += 1
            else {
              seen.add(ctx.state)
              queue.enqueue(ctx.state)
            }
          }
          catch { case t: Throwable => errors += 1 }

          iterations += 1

          if ((iterations % 1000000) == 0) {
            println("#iterations: " + iterations)
            println("#queue: " + queue.size)
            println("#unique states: " + seen.size)
            println("#cache: " + cache)
            println("#errors: " + errors)
            println()
          }
        }
      }
    )

    println("#objects: " + objects.size)
    println("#messages: " + msgGenerator.size)
    println("#iterations: " + iterations)
    println("#unique states: " + seen.size)
    println("#cache: " + cache)
    println("#errors: " + errors)

    println()
    
    println("checkNoMoneyLost: " + checkNoMoneyLostInvariant(seen))
  }

  val initialAmount = 10
  
  def checkNoMoneyLostInvariant(states: mutable.HashSet[ST]) = {
    val ctx = TestContext()

    states.forall { state =>
      
      val opened_and_closed = state.keys.toSeq.
        filter(_.isInstanceOf[Account.Id]).
        map(x => ctx.withState(state)(x.asInstanceOf[Account.Id]).obj).
        filter(obj => obj.state == "Opened" || obj.state == "Closed")

      opened_and_closed.map(x => x.data.balance).sum == (opened_and_closed.size * initialAmount)
    }
  }

  def msgSend[I <: Id[O], O, R](ids: Seq[I], msg: Seq[Message[I, O, R]]) = {
    ids.flatMap(i => msg.map(m => MsgSend(i, m))).toIndexedSeq
  }

  case class MsgSend[I <: Id[O], O, +R](id: I, msg: Message[I, O, R]) {
    def apply(implicit c: Context) = id ! msg
  }

  def accountMsg = Seq(Account.Open(initialAmount), Account.Close(), Account.ReOpen())

  def transferMsg(accounts: Seq[Account.Id]) = {
    val amounts = Array(1, 2, 5)
    
    accounts.flatMap(a1 => accounts.flatMap(a2 => amounts.map(amt => Transfer.Book(a1, a2, amt))))
  }
}