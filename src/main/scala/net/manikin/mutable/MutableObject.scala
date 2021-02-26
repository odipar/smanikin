package net.manikin.mutable

object MutableObject {
  import java.lang.reflect.Field

  trait MObject extends Cloneable {
    // mimicking this.copy(...) on non-case class, see Polymorph example
    def copy(ff: (this.type => Unit)*): this.type = {
      val cp = super.clone().asInstanceOf[this.type]
      ff.foreach(_ (cp))
      cp
    }

    // mimicking toString on non-case class
    override def toString: String = {
      getClass.getSimpleName + "(" +
        declaredFields(getClass).
          reverse.
          map { v => v.setAccessible(true); v.getName + " = " + v.get(this) }.
          mkString(", ") +
        ")"
    }

    def declaredFields(cl: Class[_]): List[Field] = {
      cl.getDeclaredFields.toList ++ { if (cl.getSuperclass != null) declaredFields(cl.getSuperclass); else List() }
    }
  }
}
