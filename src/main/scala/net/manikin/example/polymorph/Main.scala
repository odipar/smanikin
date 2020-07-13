package net.manikin.example.polymorph

object Main {
  import net.manikin.core.TransObject._
  import net.manikin.core.context.DefaultContext._
  import net.manikin.core.MutableValue._

  trait ID[+O <: Data] extends Id[O] {
    val id: Long
    def setName(name: String)(implicit c: Context): Unit = this ! SetName(name)
  }


  trait Id_A[+O <: AData] extends ID[O] {
    def setAddress(address: String)(implicit c: Context): Unit = this ! SetAddress(address)
  }

  case class IdA(id: Long) extends Id_A[AData] {
    def init = new AData{}
  }

  trait Id_B[+O <: BData] extends ID[O] {
    override def setName(name: String)(implicit c: Context): Unit = this ! SetNameB(name)
    def setAge(age: Long)(implicit c: Context): Unit = this ! SetAge(age)
  }

  case class IdB(id: Long) extends Id_B[BData] {
    def init = new BData{}
  }

  trait Id_AB[+O <: ABData] extends ID[O] with Id_A[O] with Id_B[O]

  case class IdAB(id: Long) extends Id_AB[ABData] {
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

  abstract class AbstractSetName[O <: Data](val name: String) extends SetMessage[O] {
    def pre = name != null && name.nonEmpty
    def app = obj.copy(_.name = name)
    def pst = obj.name == name
  }

  case class SetName(override val name: String) extends AbstractSetName[Data](name)

  case class SetNameB(override val name: String) extends AbstractSetName[BData](name) {
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
    
    a.setName(name = "name1")
    a.setAddress(address = "US")

    b.setName(name = "name2")
    b.setAge(age = 10)

    ab.setName(name ="name3")
    ab.setAddress(address ="US")
    ab.setAge(age = 10)

    //b.setAddress("US") // DOESN'T COMPILE
    
    println("a: " + ctx(a))
    println("b: " + ctx(b))
    println("ab: " + ctx(ab))

  }
}