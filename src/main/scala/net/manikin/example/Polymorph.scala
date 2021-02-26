package net.manikin.example

object Polymorph {
  import net.manikin.core.Core._
  import net.manikin.message.ThreadLocal._
  import net.manikin.message.Transaction._
  import net.manikin.mutable.MutableObject._
  import net.manikin.world.HistoryWorld._
  
  trait State extends MObject {
    var name: String = _
  }

  trait AState extends State {
    var address: String = _
  }

  trait BState extends State {
    var age: Long = _
  }

  trait ABState extends AState with BState

  trait SetMessage[W <: World[W], O <: State] extends LMsg[W, Id[O], O, Unit] {
    def eff = {}
  }

  class SetName[W <: World[W], O <: State](val name: String) extends SetMessage[W, O] {
    def pre = name != null && name.nonEmpty
    def app = obj.copy(_.name = name)
    def pst = obj.name == name
  }

  case class SetNameB[W <: World[W], O <: State](override val name: String) extends SetName[W, O](name) {
    override def pre = super.pre && name.length > 1
  }

  trait ID[+O <: State] extends Id[O] {
    val id: Long

    def send[W <: World[W], O2 >: O, R](msg: LMsg[W, Id[O2], O2, R]) = msg.send[Id[O2], O2, R](this, msg)
    def setName[W <: World[W]](name: String): Unit = send(new SetName[W, O](name))
  }

  trait Id_A extends ID[AState] {
    def setAddress[W <: World[W]](address: String) = send(SetAddress(address))
  }

  case class IdA(id: Long) extends Id_A {
    def init = new AState {}
  }

  trait Id_B extends ID[BState] {
    def setAge[W <: World[W]](age: Long): Unit = send(SetAge(age))
    override def setName[W <: World[W]](name: String) = send(SetNameB[W, BState](name))
  }

  case class IdB(id: Long) extends Id_B {
    def init = new BState {}
  }

  trait Id_AB extends ID[ABState] with Id_A with Id_B

  case class IdAB(id: Long) extends Id_AB {
    def init = new ABState {}
  }

  case class SetAddress[W <: World[W]](address: String) extends SetMessage[W, AState] {
    def pre = address != null && address.nonEmpty
    def app = obj.copy(_.address = address)
    def pst = obj.address == address
  }

  case class SetAge[W <: World[W]](age: Long) extends SetMessage[W, BState] {
    def pre = age >= 0
    def app = obj.copy(_.age = age)
    def pst = obj.age == age
  }

  case class T[W <: World[W]]() extends Do[W, Unit] {
    def eff = {
      val a = IdA(1)
      val b = IdB(2)
      val ab = IdAB(3)   // multiple inheritance

      a.setName("name1")
      a.setAddress("US")

      b.setName("name2")
      b.setAge(10)

      ab.setName("name3")
      ab.setAddress("EU")
      ab.setAge(20)
    }
  }

  def main(args: Array[String]): Unit = {
    val value = HistoryWorld().send(TID(), T())
    println("c2: " + value.world.state)
  }
}