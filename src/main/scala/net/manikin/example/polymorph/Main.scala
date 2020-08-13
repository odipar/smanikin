package net.manikin.example.polymorph

object Main {
  import net.manikin.core.TransObject._
  import net.manikin.core.context.EventContext._
  import net.manikin.core.MutableValue._

  trait State extends MValue {
    var name: String = ""
  }

  trait AState extends State {
    var address: String = ""
  }

  trait BState extends State {
    var age: Long = 0
  }

  trait ABState extends AState with BState

  trait SetMessage[O <: State] extends Message[ID[O], O, Unit] {
    def eff = {}
  }

  class SetName[O <: State](val name: String) extends SetMessage[O] {
    def pre = name != null && name.nonEmpty
    def app = obj.copy(_.name = name)
    def pst = obj.name == name
  }

  case class SetNameB(override val name: String) extends SetName[BState](name) {
    override def pre = super.pre && name.length > 1
  }

  case class SetAddress(address: String) extends SetMessage[AState] {
    def pre = address != null && address.nonEmpty
    def app = obj.copy(_.address = address)
    def pst = obj.address == address
  }

  case class SetAge(age: Long) extends SetMessage[BState] {
    def pre = age >= 0
    def app = obj.copy(_.age = age)
    def pst = obj.age == age
  }

  trait ID[+O <: State] extends Id[O] {
    type C = Context

    val id: Long
    def setName(name: String)(implicit c: C): Unit = this ! new SetName[O](name)
  }


  trait Id_A extends ID[AState] {
    def setAddress(address: String)(implicit c: C): Unit = this ! SetAddress(address)
  }

  case class IdA(id: Long) extends Id_A {
    def init = new AState{}
  }

  trait Id_B extends ID[BState] {
    def setAge(age: Long)(implicit c: C): Unit = this ! SetAge(age)
    override def setName(name: String)(implicit c: C): Unit = this ! SetNameB(name)
  }

  case class IdB(id: Long) extends Id_B {
    def init = new BState{}
  }

  trait Id_AB extends ID[ABState] with Id_A with Id_B

  case class IdAB(id: Long) extends Id_AB {
    def init = new ABState{}
  }
  
  def main(args: Array[String]): Unit = {
    implicit val ctx = new EventContext()

    val a = IdA(1)
    val b = IdB(1)
    val ab = IdAB(1)

    ab ! SetAge(1)
    
    a.setName(name = "name1")
    a.setAddress(address = "US")

    b.setName(name = "name2")
    b.setAge(age = 10)

    ab.setName(name ="name3")
    ab.setAddress(address ="US")
    ab.setAge(age = 10)

    //
    // b.setAddress("US") // DOESN'T COMPILE
    
    println("a: " + ctx(a))
    println("b: " + ctx(b))
    println("ab: " + ctx(ab))

  }
}