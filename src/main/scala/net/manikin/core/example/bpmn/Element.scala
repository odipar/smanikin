package net.manikin.core.example.bpmn

object Element {
  import net.manikin.core.asm.AbstractStateMachine._

  case class ElementData[+X](name: String, element: X)

  type TRACE = Seq[Seq[ElementId[Any]]]
  
  trait ElementId[+X] extends Id[ElementData[X]] {
    def init = ElementData(name = null, initElement)
    def initElement: X

    def traces(implicit ctx: Context) : TRACE = Seq(Seq(this))
    def insert(before: ElementId[Any], after: ElementId[Any])(implicit ctx: Context): Unit = { }
    def contains(other: ElementId[Any])(implicit ctx: Context) : Boolean = this == other

    def product(i: ElementId[_], s: Seq[TRACE]): TRACE = {
      s.fold(Seq(Seq(i)))((x, y) => x.flatMap(xx => y.map(yy => xx ++ yy))).map(x => x ++ Seq(i))
    }
  }

  case class EId[+X](self: Id[ElementData[X]])

  implicit class EIdContext[X](id: EId[X]) {
    def apply()(implicit ctx: Context): X = id.self().element
    def update(x: X)(implicit ctx: Context): Unit = id.self() = id.self().copy(element = x)
    def previous()(implicit ctx: Context): X = id.self.prev.element
  }
  
  trait ElementTrs[+X] extends Transition[ElementData[X]] {
    def name = self().name
    def element = EId(self)
  }
  
  case class SetName(new_name: String) extends ElementTrs[Any] {
    def pre = true
    def app = self() = self().copy(name = new_name)
    def pst = name == new_name
  }
}
