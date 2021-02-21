package net.manikin.main

object Base {

  import scala.language.implicitConversions

  trait Id[+O] {
    def init: O
  }

  trait Self[I <: Id[O], O] {
    def id: I
    def obj: O
  }

  trait Prev[I <: Id[O], O] extends Self[I, O] {
    def old: O
  }

  trait CTypes[C <: Context[C]] {
    type E[+X] = Expr[C, X]
  }

  trait MTypes[C <: Context[C], I <: Id[O], O] extends CTypes[C] {
    type S = Self[I, O]
    type P = Prev[I, O]
  }

  trait Msg[C <: Context[C], I <: Id[O], O, +R] extends MTypes[C, I, O]{
    def preCondition: S => E[Boolean]
    def apply: S => E[O]
    def effect: S => E[R]
    def postCondition: P => E[Boolean]
  }

  trait Context[C <: Context[C]] extends CTypes[C] {
    def self: C
    def eval[A](e: E[A]): (C, A) = e.eval(self)

    def apply[A](x: A): E[A] = VAL(x)
    def get[O](id: Id[O]): (C, O)
    def previous[O](id: Id[O]): (C, O)
    def send[I <: Id[O], O, R](id: I, msg: Msg[C, I, O, R]): (C, R)
  }

  type E[C <: Context[C], +A] = Expr[C, A]

  trait Expr[C <: Context[C], +A] extends CTypes[C] {
    def eval(c: C): (C, A)

    def map[B](f: A => B): E[B] = MAP(this, f)
    def flatMap[B](f: A => E[B]): E[B] = FLATMAP(this, f)
    def THEN[B](e1: E[B]): E[B] = THEN_(this, e1)
    def ![I <: Id[O], O, R](msg: Msg[C, I, O, R])(implicit ev: E[A] <:< E[I]): E[R] = SEND(ev(this), VAL(msg))
    def obj2[O](implicit ev: E[A] <:< E[Id[O]]): E[O] = GET[C, Id[O], O](ev(this))
    def old2[I <: Id[O], O](implicit ev: E[A] <:< E[I]): E[O] = PREVIOUS[C, I, O](ev(this))
  }

  case class VAL[C <: Context[C], A](x: A) extends E[C, A] {
    def eval(c: C) = (c, x)
  }
  case class MAP[C <: Context[C], A, B](x: E[C, A], f: A => B) extends E[C, B] {
    def eval(c: C) = { val (c2, a) = c.eval(x) ; (c2, f(a)) }
  }
  case class FLATMAP[C <: Context[C], A, B](x: E[C, A], f: A => E[C, B]) extends E[C, B] {
    def eval(c: C) = { val (c2, a) = c.eval(x) ; c2.eval(f(a)) }
  }
  case class BIN[C <: Context[C], A, B, Z](x1: E[C, A], x2: E[C, B], f: (A, B) => Z) extends E[C, Z] {
    def eval(c: C) = { val (c2, a) = c.eval(x1) ; val(c3, b) = c2.eval(x2) ; (c3, f(a, b)) }
  }
  case class THEN_[C <: Context[C], A, B](x1: E[C, _], x2: E[C, B]) extends E[C, B] {
    def eval(c: C) = { val (c2, _) = c.eval(x1) ; c2.eval(x2) }
  }
  case class SEND[C <: Context[C], I <: Id[O], O, R](id: E[C, I], msg: E[C, Msg[C, I, O, R]]) extends E[C, R] {
    def eval(c: C) = {val (c2, i) = c.eval(id); val (c3, m) = msg.eval(c2); c2.send(i, m)}
  }
  case class GET[C <: Context[C], I <: Id[O], O](id: E[C, I]) extends E[C, O] {
    def eval(c: C) = { val (c2, i) = c.eval(id) ; c2.get(i) }
  }
  case class PREVIOUS[C <: Context[C], I <: Id[O], O](id: E[C, I]) extends E[C, O] {
    def eval(c: C) = { val (c2, i) = c.eval(id) ; c2.get(i) }
  }

  implicit class ObjectSyntax[C <: Context[C], A](val t: E[C, A]) {
    def ===(o: E[C, A]): E[C, Boolean] = BIN[C, A, A, Boolean](t, o, _==_)
  }

  implicit class BoolSyntax[C <: Context[C]](val t: E[C, Boolean]) {
    type B = E[C, Boolean]

    def b[Z](o: B, f: (Boolean, Boolean) => Z): E[C, Z] = BIN(t,o,f)

    def &&(o: B) = b(o,_&&_)
    def ||(o: B) = b(o,_||_)
    def ^ (o: B) = b(o,_^_)

    def unary_! = t.map(!_)
  }

  implicit class IntSyntax[C <: Context[C]](val t: E[C, Int]) {
    type I = E[C, Int]

    def i[Z](o: I, f: (Int, Int) => Z): E[C, Z] = BIN(t,o,f)

    def +(o: I) = i(o, _+_)
    def *(o: I) = i(o, _*_)
    def -(o: I) = i(o, _-_)
    def /(o: I) = i(o, _/_)
    def ^(o: I) = i(o, _^_)
    def >(o: I) = i(o, _>_)
    def <(o: I)= i(o, _<_)
    def <=(o: I) = i(o, _<=_)
    def >=(o: I) = i(o, _>=_)
    def unary_- = t.map(-_)
  }

  implicit class DoubleSyntax[C <: Context[C]](val t: E[C, Double]) {
    type D = E[C, Double]

    def d[Z](o: D, f: (Double, Double) => Z): E[C, Z] = BIN(t,o,f)

    def +(o: D) = d(o, _+_)
    def *(o: D) = d(o, _*_)
    def -(o: D) = d(o, _-_)
    def /(o: D) = d(o, _/_)
    def >(o: D)= d(o, _>_)
    def <(o: D) = d(o, _<_)
    def <=(o: D) = d(o, _<=_)
    def >=(o: D) = d(o, _>=_)
    def unary_- = t.map(-_)
  }

  case class SContext(prev: SContext = null, state: Map[Id[_], _] = Map()) extends Context[SContext] {
    def self = this

    def get[O](id: Id[O]) = (this, state.getOrElse(id, id.init).asInstanceOf[O])
    def previous[O](id: Id[O]) = (this, prev.get(id)._2)
    def send[I <: Id[O], O, R](id: I, msg: Msg[SContext, I, O, R]): (SContext, R) = {
      val vid = SSelf(id, get(id)._2)

      val (c2, pre_) = msg.preCondition(vid).eval(this)
      if (!pre_) sys.error("Pre-condition failed")
      else {
        val (_, app_) = msg.apply(vid).eval(c2)  // ignore any side effects
        val (c4, eff_) = msg.effect(vid).eval(SContext(this, state + (id -> app_)))
        val (_, pst_) = msg.postCondition(SPrev(id, app_, vid.obj)).eval(SContext(this, c4.state))
        if (!pst_) sys.error("Post-condition failed")
        else (c4, eff_)
      }
    }

    case class SSelf[I <: Id[O], O](id: I, obj: O) extends Self[I, O]
    case class SPrev[I <: Id[O], O](id: I, obj: O, old: O) extends Prev[I, O]
  }

  val threadSelf = new ThreadLocal[Self[_, _]]
  val threadPrev = new ThreadLocal[Prev[_, _]]

  trait TMsg[C <: Context[C], I <: Id[O], O, +R] extends Msg[C, I, O, R] {
    implicit def lift[A](a: A): E[A] = VAL(a)
    implicit class IdSyntax[O2](val id: Id[O2]) {
      def obj[X](f: O2 => X) = MAP(GET[C, Id[O2], O2](id), f)
      def old[X](f: O2 => X) = MAP(PREVIOUS[C, Id[O2], O2](id), f)
    }

    def preCondition2: E[Boolean]
    def apply2: E[O]
    def effect2: E[R]
    def postCondition2: E[Boolean]

    def preCondition = s => self(s, preCondition2)
    def apply = s => self(s, apply2)
    def effect = s => self(s, effect2)
    def postCondition = p => prev(p, postCondition2)

    private def setSelf(s: S): Unit = threadSelf.set(s)
    private def getSelf: S = threadSelf.get().asInstanceOf[Self[I, O]]
    private def setPrev(s: P): Unit = { setSelf(s); threadPrev.set(s) }
    private def getPrev: P = threadPrev.get().asInstanceOf[Prev[I, O]]

    private def self[X](s: S, f: => E[X]) = try { setSelf(s); f } finally setSelf(null)
    private def prev[X](s: P, f: => E[X]) = try { setPrev(s); f } finally setPrev(null)

    def obj: O = getSelf.obj
    def old: O = getPrev.old
    def self: I = getSelf.id
  }

  trait AMsg[C <: Context[C], I <: Id[O], O, +R] extends TMsg[C, I, O, R] {
    def pre: E[Boolean]
    def app: E[O]
    def eff: E[R]
    def pst: E[Boolean]

    def preCondition2 = pre
    def apply2 = app
    def effect2 = eff
    def postCondition2 = pst
  }

  case class AccountId(iban: String) extends Id[Account] {
    def init = Account()
  }

  case class Account(balance: Double = 0.0)

  case class Open[C <: Context[C]](initial: Double) extends AMsg[C, AccountId, Account, Unit] {
    def pre = initial > 0.0
    def app = Account(initial)
    def eff = { }
    def pst = obj.balance == initial
  }

  case class Withdraw[C <: Context[C]](amount: Double) extends AMsg[C, AccountId, Account, Unit] {
    def pre = amount > 0.0 && obj.balance > amount
    def app = Account(obj.balance - amount)
    def eff = { }
    def pst = obj.balance == old.balance - amount
  }

  case class Deposit[C <: Context[C]](amount: Double) extends AMsg[C, AccountId, Account, Unit] {
    def pre = amount > 0.0
    def app = Account(obj.balance + amount)
    def eff = { }
    def pst = obj.balance == old.balance + amount
  }

  case class TransferId(id: Long) extends Id[Transfer] {
    def init = Transfer()
  }

  case class Transfer(from: AccountId = null, to: AccountId = null, amount: Double = 0)

  val balance = (a: Account) => a.balance

  case class Book[C <: Context[C]](from: AccountId = null, to: AccountId = null, amount: Double = 0) extends AMsg[C, TransferId, Transfer, Unit] {
    def pre = amount > 0.0
    def app = Transfer(from, to, amount)
    def eff = from ! Withdraw(amount) THEN to ! Deposit(amount)
    def pst = from.obj(balance) + to.obj(balance) === from.old(balance) + to.old(balance)
  }

  def main(args: Array[String]): Unit = {
    val c = SContext()

    val a1 = AccountId("A1")
    val a2 = AccountId("A2")
    val t1 = TransferId(1)

    val e = for {
      _ <- c(a1) ! Open(50)
      _ <- c(a2) ! Open(80)
      _ <- c(t1) ! Book(a1, a2, 30)
    } yield()

    val r = e.eval(c)
    println("r: " + r._1.state)
  }
}
