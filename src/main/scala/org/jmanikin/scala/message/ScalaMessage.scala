package org.jmanikin.scala.message

object ScalaMessage {
  import org.jmanikin.core._
  import org.jmanikin.message._

  // Redefined builder stages
  trait SPre[W <: World[W], I <: Id[O], O, R] { def pre(pre: => Boolean): SApp[W, I, O, R] }
  trait SApp[W <: World[W], I <: Id[O], O, R] { def app(pre: => O): SEff[W, I, O, R] }
  trait SEff[W <: World[W], I <: Id[O], O, R] { def eff(pre: => R): SPst[W, I, O, R] }
  trait SPst[W <: World[W], I <: Id[O], O, R] { def pst(pre: => Boolean): SMsg[W, I, O, R] }
  trait SMsg[W <: World[W], I <: Id[O], O, R] { def msg: Msg[W, I, O, R] }

  // ScalaMessage 'wraps' LocalMessage to allow call-by name Scala syntax
  trait ScalaMessage[W <: World[W], I <: Id[O], O, R] extends LocalMessage[W, I, O, R] {
    def local = scala.msg
    def scala: SMsg[W, I, O, R]
    def pre(pre: => Boolean) = E(env()).pre(pre)

    private case class E(_env: Environment[W, I, O, R]) extends
      SPre[W, I, O, R] with SApp[W, I, O, R] with SEff[W, I, O, R] with SPst[W, I, O, R] with SMsg[W, I, O, R]
    {
      private var _app: Apply[W, I, O, R] = _
      private var _eff: Effect[W, I, O, R] = _
      private var _pst: PostCondition[W, I, O, R] = _
      private var _msg: Msg[W, I, O, R] = _

      def pre(pre: => Boolean): SApp[W, I, O, R] =  { _app = _env.pre(() => pre) ; this }
      def app(app: => O): SEff[W, I, O, R] =        { _eff = _app.app(() => app) ; this }
      def eff(eff: => R): SPst[W, I, O, R]  =       { _pst = _eff.eff(() => eff) ; this }
      def pst(pst: => Boolean): SMsg[W, I, O, R] =  { _msg = _pst.pst(() => pst) ; this }

      def msg = _msg
    }
  }
}
