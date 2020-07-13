package net.manikin.core

object MutableValue {
  import java.lang.reflect.Field

  class MValue extends Cloneable {
    // mimicking this.copy(...) on non-case class
    def copy(f: this.type => Unit): this.type = {
      val cp = clone().asInstanceOf[this.type]
      f(cp)
      cp
    }
    
    // mimicking toString on non-case class
    override def toString: String = {
      getClass.getSimpleName + "(" +
        declaredFields(getClass).
          reverse.
          map{v => v.setAccessible(true) ; v.getName + " = " + v.get(this)}.
          mkString(", ") +
        ")"
    }
    
    def declaredFields(cl: Class[_]): List[Field] = {
      cl.getDeclaredFields.toList ++
      { if (cl.getSuperclass != null) declaredFields(cl.getSuperclass) ; else List() }
    }
  }
}
