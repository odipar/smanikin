package net.manikin.example.polymorph


object Main {
  import net.manikin.core.TransObject._
  import net.manikin.core.context.DefaultContext._
  
  trait IdBase[O <: DataBase] extends Id[O]

  case class IdA(i: Long) extends IdBase[A] {
    def init = new A
  }

  case class IdB(i: Long) extends IdBase[B] {
    def init = new B
  }

  class DataBase extends Cloneable {
    var name: String = ""
    
    // mimicking this.copy(...) on non-case classes
    def copy(f: this.type => Unit): this.type = { val cp = clone().asInstanceOf[this.type] ; f(cp) ; cp }
  }

  class A extends DataBase {
    var address: String = ""

    override def toString = "A(" + name + "," + address + ")"
  }

  class B extends DataBase {
    var age: Long = 0
    
    override def toString = "B(" + name + "," + age + ")"
  }

  trait SetMessage[O <: DataBase] extends Message[IdBase[O], O, Unit] {
    def pre = true
    def eff = { }
    def pst = true
  }
  
  case class SetName(name: String) extends SetMessage[DataBase] {
    def app = obj.copy(_.name = name)
  }

  case class SetAddress(address: String) extends SetMessage[A] {
    def app = obj.copy(_.address = address)
  }

  case class SetAge(age: Long) extends SetMessage[B] {
    def app = obj.copy(_.age = age)
  }

  def main(args: Array[String]): Unit = {
    implicit val ctx = new DefaultContext()

    val a = IdA(1)
    val b = IdB(1)

    a ! SetName("name1")
    b ! SetName("name2")

    a ! SetAddress("US")
    b ! SetAge(10)

    // b ! SetAddress("US") // DOESN'T COMPILE
    
    println("a: " + ctx(a))
    println("b: " + ctx(b))
  }
}