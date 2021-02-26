package net.manikin.core

object Core {

  // Identifier and factory for pristine Object O
  trait Id[+O] {
    def init: O
  }
  
  /* In the scope of World W, a Message can be sent to an Object O with identifier I, returning R
   *
   * There are four stages:
   *
   * 1) preCondition: predicate over anything. MUST BE a pure function (no side effects) and evaluate to true
   * 2) apply: returns (a new version of) Object O. MUST BE a pure function (used for Event sourcing)
   * 3) effect: can do anything and returns R
   * 4) postCondition: predicate over anything, including old states. MUST BE a pure function and evaluate to true
   */
  trait Message[W <: World[W], I <: Id[O], O, R] extends ValType[W] {
    def preCondition: V[I] => V[Boolean]
    def apply: V[I] => V[O]
    def effect: V[I] => V[R]
    def postCondition: V[I] => V[Boolean]
  }

  // A World maps Ids to (old) Objects and can send Messages
  trait World[W <: World[W]] extends ValType[W] {
    def obj[O](id: Id[O]): V[O]
    def old[O](id: Id[O]): V[O]
    def send[I <: Id[O], O, R](id: I, msg: Message[W, I, O, R]): V[R]
  }

  // Worlds are threaded through Values (explicit monad)
  case class Value[W <: World[W], V](world: W, value: V) extends World[W] {
    def obj[O](id: Id[O]) = world.obj(id)
    def old[O](id: Id[O]) = world.old(id)
    def send[I <: Id[O], O, R](id: I, msg: Message[W, I, O, R]) = world.send(id, msg)
  }

  // Convenient type alias (less typing on keyboard)
  trait ValType[W <: World[W]] {
    type V[X] = Value[W, X]
  }
}
