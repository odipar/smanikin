package net.manikin.test

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class SimpleAccountSpec extends AnyWordSpec with Matchers {
  import net.manikin.example.bank.Account
  import net.manikin.example.bank.Transfer
  import net.manikin.example.bank.IBAN._
  import net.manikin.core.context.ObjectContext._

  "Accounts should" should {

    "accept two valid Transfers in the same Context" in {
      implicit val ctx = new ObjectContext()

      val a1 = Account.Id(IBAN("A1"))
      val a2 = Account.Id(IBAN("A2"))
      val t1 = Transfer.Id(1)
      val t2 = Transfer.Id(2)

      a1 ! Account.Open(100)
      a2 ! Account.Open(50)
      t1 ! Transfer.Book(a1, a2, 20)
      t2 ! Transfer.Book(a2, a1, 30)

      a1.data.balance shouldBe 110 // 100 - 20 + 30
      a2.data.balance shouldBe 40  //  50 + 20 - 30
      t1.state shouldBe "Booked"
      t2.state shouldBe "Booked"
    }
  }
}