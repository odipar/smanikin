package net.manikin.main

import net.manikin.core.context.DefaultContext.DefaultContext
import net.manikin.example.bank.{Account, Transfer}
import net.manikin.example.bank.IBAN.IBAN
import net.manikin.serialization.SerializationUtils

object Main {
  import net.manikin.example._
  import scala.language.implicitConversions
  
  def main(args: Array[String]): Unit = {
    implicit val ctx = new DefaultContext()

    val a1 = Account.Id(IBAN("A1"))
    val a2 = Account.Id(IBAN("A2"))

    a1 ! Account.Open(100)
    a2 ! Account.Open(80)

    val nr = 1000000

    SerializationUtils.time {
      for (i <- 1 to nr) {
        val t1 = Transfer.Id(i)
        val t2 = Transfer.Id(nr + i)
        t1 ! Transfer.Book(a1, a2, 10)
        t2 ! Transfer.Book(a2, a1, 10)
        if ((i % 100000) == 0) println("i: " + i)
      }
    }
  }
}