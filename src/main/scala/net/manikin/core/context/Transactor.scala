package net.manikin.core.context

object Transactor {
  import net.manikin.core.TransObject._
  import net.manikin.core.context.StoreWorld._
  import java.util.UUID

  // A Transactor commits to a StoreWorld. Upon failure it retries.
  case class Transactor(private var ctx: StoreWorld = new StoreWorld()) {
    def apply[O](id: Id[O]): VObject[O] = ctx.apply(id)
    def commit[I <: Id[O], O, R](id: I, message: Message[I, O, R]): R = commit(id, message, 3)
    def commit[I <: Id[O], O, R](id: I, message: Message[I, O, R], retry: Int): R = {
      val new_context = ctx.copyThis()

      try {
        val result = new_context.send(id, message)
        new_context.commit()
        ctx = new_context
        result
      }
      catch {
        case e: Throwable => {
          if (retry > 0) {
            val different = ctx.retry(new_context)
            if (ctx.failures(id) > 1 && !different) throw e
            commit(id, message, retry - 1)
          }
          else throw e
        }
      }
    }
  }

  case class TId(uuid: UUID = UUID.randomUUID()) extends Id[Unit] { def init: Unit = { } }

  trait Transaction[+R] extends Message[TId, Unit, R] {
    def pre = true
    def app = { }
    def pst = true
  }

  object An_Untyped_Purely_Functional_Model_Of_Manikin {
    type State = Any
    type Result = Any

    trait Id {
      def initialState: State
    }

    trait Message {
      def preCondition(current: World): Boolean
      def apply(current: State): State
      def effect(current: World): (Result, World)
      def postCondition(previous: World, current: World): Boolean
    }
    
    trait World extends Map[Id, State] {
      def self: Id
      def send(id: Id, msg: Message): World

      def commit(other: World): World
      def refresh(from: World, ids: Set[Id]): World
      
      def parents: Set[World]
    }

    trait ErrorWorld extends World {
      def error: Throwable
    }
  }
  
  object A_Typed_Purely_Functional_Model_Of_Manikin {
    trait Id[S] {
      def initialState: S
    }

    trait Message[I <: Id[S], S, R] {
      def preCondition(current: World[S]): Boolean
      def apply(current: S): S
      def effect(current: World[S]): (R, World[S])
      def postCondition(previous: World[S], current: World[S]): Boolean
    }
    
    trait World[S] {
      def self: Id[S]
      def parent: World[_]
      
      def send[I <: Id[S2], S2, R](id: I, msg: Message[I, S2, R]): World[S2]
      def commit(other: World[_]): World[S]
      def refresh(from: World[_], ids: Set[Id[_]]): World[S]
    }

    trait ErrorWorld[S] extends World[S] {
      def error: Throwable                       
    }
  }
}