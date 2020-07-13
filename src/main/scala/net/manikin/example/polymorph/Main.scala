package net.manikin.example.polymorph

object Main {
  import net.manikin.core.TransObject._
  import net.manikin.core.context.DefaultContext._
  import net.manikin.core.MutableValue._

  abstract class ID[+O <: Data](val id: Long) extends Id[O] {
    def setName(name: String)(implicit c: Context) = this ! new SetName[O](name)
  }

  case class IdA(override val id: Long) extends ID[AData](id) {
    def init = new AData{}
  }

  case class IdB(override val id: Long) extends ID[BData](id) {
    def init = new BData{}
    override def setName(name: String)(implicit c: Context) = this ! SetNameB(name)
  }

  case class IdAB(override val id: Long) extends ID[ABData](id) {
    def init = new ABData{}
  }

  trait Data extends MValue {
    var name: String = ""
  }

  trait AData extends Data {
    var address: String = ""
  }

  trait BData extends Data {
    var age: Long = 0
  }

  trait ABData extends AData with BData

  trait SetMessage[O <: Data] extends Message[ID[O], O, Unit] {
    def eff = { }
  }

  class SetName[O <: Data](val name: String) extends SetMessage[O] {
    def pre = name != null && name.nonEmpty
    def app = obj.copy(_.name = name)
    def pst = obj.name == name
  }

  case class SetNameB(override val name: String) extends SetName[BData](name) {
    override def pre = super.pre || name.length > 1
  }

  case class SetAddress(address: String) extends SetMessage[AData] {
    def pre = address != null && address.nonEmpty
    def app = obj.copy(_.address = address)
    def pst = obj.address == address
  }

  case class SetAge(age: Long) extends SetMessage[BData] {
    def pre = age >= 0
    def app = obj.copy(_.age = age)
    def pst = obj.age == age
  }

  def main(args: Array[String]): Unit = {
    implicit val ctx = new DefaultContext()

    val a = IdA(1)
    val b = IdB(1)
    val ab = IdAB(1)

    a.setName("name1")
    a ! SetAddress("US")

    b.setName("name2")
    b ! SetAge(10)

    ab.setName("name3")
    ab ! SetAddress("US")
    ab ! SetAge(10)

    //b ! SetAddress("US") // DOESN'T COMPILE
    
    println("a: " + ctx(a))
    println("b: " + ctx(b))
    println("ab: " + ctx(ab))

  }
}