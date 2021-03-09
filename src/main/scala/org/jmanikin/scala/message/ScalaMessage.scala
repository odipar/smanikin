package org.jmanikin.scala.message

object ScalaMessage {
  import org.jmanikin.core._
  import org.jmanikin.message._

  // Redefined builder stages
  trait SPre[I <: Id[O], O, E] { def pre(pre: => Boolean): SApp[ I, O, E] }
  trait SApp[I <: Id[O], O, E] { def app(app: => O):       SEff[I, O, E] }
  trait SEff[I <: Id[O], O, E] { def eff(eff: => E):       SPst[I, O, E] }
  trait SPst[I <: Id[O], O, E] { def pst(pst: => Boolean): SMsg[I, O, E] }
  trait SMsg[I <: Id[O], O, E] { def msg:                  Msg[I, O, E] }

  // ScalaMessage 'wraps' LocalMessage to allow call-by name Scala syntax
  trait ScalaMessage[I <: Id[O], O, E] extends LocalMessage[I, O, E] {
    def local = scala.msg
    def scala: SMsg[I, O, E]
    def pre(pre: => Boolean) = ScalaEnv(env()).pre(pre)

    private case class ScalaEnv(_env: Environment[I, O, E]) extends
        SPre[I, O, E] with SApp[I, O, E] with SEff[I, O, E] with SPst[I, O, E] with SMsg[I, O, E]
    {
      private var _app: Apply[I, O, E] = _
      private var _eff: Effect[I, O, E] = _
      private var _pst: PostCondition[I, O, E] = _
      private var _msg: Msg[I, O, E] = _

      def pre(pre: => Boolean): SApp[I, O, E] = { _app = _env.pre(() => pre) ; this }
      def app(app: => O):       SEff[I, O, E] = { _eff = _app.app(() => app) ; this }
      def eff(eff: => E):       SPst[I, O, E] = { _pst = _eff.eff(() => eff) ; this }
      def pst(pst: => Boolean): SMsg[I, O, E] = { _msg = _pst.pst(() => pst) ; this }

      def msg = _msg
    }
  }
}
