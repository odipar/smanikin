package org.jmanikin.scala.message

object ScalaMessage {
  import org.jmanikin.core._
  import org.jmanikin.message._

  // Redefined builder stages
  trait SPre[W <: World[W], I <: Id[O], O, E] { def pre(pre: => Boolean): SApp[W, I, O, E] }
  trait SApp[W <: World[W], I <: Id[O], O, E] { def app(app: => O):       SEff[W, I, O, E] }
  trait SEff[W <: World[W], I <: Id[O], O, E] { def eff(eff: => E):       SPst[W, I, O, E] }
  trait SPst[W <: World[W], I <: Id[O], O, E] { def pst(pst: => Boolean): SMsg[W, I, O, E] }
  trait SMsg[W <: World[W], I <: Id[O], O, E] { def msg:                  Msg[W, I, O, E] }

  // ScalaMessage 'wraps' LocalMessage to allow call-by name Scala syntax
  trait ScalaMessage[W <: World[W], I <: Id[O], O, E] extends LocalMessage[W, I, O, E] {
    def local = scala.msg
    def scala: SMsg[W, I, O, E]
    def pre(pre: => Boolean) = ScalaEnv(env()).pre(pre)

    private case class ScalaEnv(_env: Environment[W, I, O, E]) extends
        SPre[W, I, O, E] with SApp[W, I, O, E] with SEff[W, I, O, E] with SPst[W, I, O, E] with SMsg[W, I, O, E]
    {
      private var _app: Apply[W, I, O, E] = _
      private var _eff: Effect[W, I, O, E] = _
      private var _pst: PostCondition[W, I, O, E] = _
      private var _msg: Msg[W, I, O, E] = _

      def pre(pre: => Boolean): SApp[W, I, O, E] = { _app = _env.pre(() => pre) ; this }
      def app(app: => O):       SEff[W, I, O, E] = { _eff = _app.app(() => app) ; this }
      def eff(eff: => E):       SPst[W, I, O, E] = { _pst = _eff.eff(() => eff) ; this }
      def pst(pst: => Boolean): SMsg[W, I, O, E] = { _msg = _pst.pst(() => pst) ; this }

      def msg = _msg
    }
  }
}
