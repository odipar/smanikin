package net.manikin.core

object Core {
  trait Id[+O] {
    def init: O
  }

  trait ValType[W <: World[W]] {
    type V[X] = Val[W, X]
  }

  trait Message[W <: World[W], I <: Id[O], O, R] extends ValType[W] {
    def preCondition: V[I] => V[Boolean]
    def apply: V[I] => V[O]
    def effect: V[I] => V[R]
    def postCondition: V[I] => V[Boolean]
  }

  trait World[W <: World[W]] extends ValType[W] {
    def obj[O](id: Id[O]): V[O]
    def old[O](id: Id[O]): V[O]
    def send[I <: Id[O], O, R](id: I, msg: Message[W, I, O, R]): V[R]
  }

  case class Val[W <: World[W], V](world: W, value: V) extends World[W] {
    def obj[O](id: Id[O]) = world.obj(id)
    def old[O](id: Id[O]) = world.old(id)
    def send[I <: Id[O], O, R](id: I, msg: Message[W, I, O, R]) = world.send(id, msg)
  }
}
