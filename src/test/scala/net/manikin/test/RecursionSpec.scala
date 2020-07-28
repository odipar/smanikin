package net.manikin.test

import net.manikin.core.TransObject.{Context, Id, Message}
import net.manikin.core.context.EventContext.EventContext
import net.manikin.core.context.ObjectContext.ObjectContext
import net.manikin.core.context.StoreContext.StoreContext
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class RecursionSpec extends AnyWordSpec with Matchers {
  case class Factorial(f: BigInt) extends Id[BigInt] {
    def init = 1
  }

  case class Calculate(c: BigInt = 1) extends Message[Factorial, BigInt, Unit] {
    def pre = true
    def app = obj * c
    def eff = if (c < self.f) self ! Calculate(c + 1)
    def pst = obj == old_obj * (c to self.f).product
  }

  def testRecursion(implicit c: Context): Unit = {
    val fac10 = Factorial(10)
    val fac500 = Factorial(500)

    fac10! Calculate()
    fac500 ! Calculate()

    c(fac10).obj shouldBe (BigInt(1) to BigInt(10)).product
    c(fac500).obj shouldBe (BigInt(1) to BigInt(500)).product
  }

  "Recursion with references to old state should not throw Exception" should {
    "for ObjectContext" in { testRecursion(new ObjectContext()) }
    "for EventContext" in { testRecursion(new EventContext()) }
    "for StoreContext" in { testRecursion(new StoreContext()) }
  }
}