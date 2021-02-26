package net.manikin.example.bank

object SimpleTransfer {
  import net.manikin.world.SimpleWorld._

  def main(args: Array[String]): Unit = {
    val a1 = Account.Id("A1")
    val a2 = Account.Id("A2")
    val t1 = Transfer.Id(1)

    val result = SimpleWorld().
      send(a1, Account.Open(50)).
      send(a2, Account.Open(80)).
      send(t1, Transfer.Book(a1, a2, 30))

    println(result.obj(a1).value) // SObject(Opened, Account(20.0))
    println(result.obj(a2).value) // SObject(Opened, Account(110.0))
  }
}
