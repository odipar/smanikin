package net.manikin.main

object Base {

  import scala.language.implicitConversions

  trait Id[+O] {
    def init: O
  }

  trait CTypes[C <: Context[C]] {
    type E[+X] = Expr[C, X]
  }

  trait Msg[C <: Context[C], I <: Id[O], O, +R] extends CTypes[C] {
    def preCondition: E[I] => E[Boolean]
    def apply: E[I] => E[O]
    def effect: E[I] => E[R]
    def postCondition: E[I] => E[Boolean]
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

    def apply[B](f: A => B): E[B] = MAP(this, f)
    def map[B](f: A => B): E[B] = MAP(this, f)
    def flatMap[B](f: A => E[B]): E[B] = FLATMAP(this, f)
    def THEN[B](e1: E[B]): E[B] = THEN_(this, e1)

    def ![I <: Id[O], O, R](msg: Msg[C, I, O, R])(implicit ev: E[A] <:< E[I]): E[R] = SEND(ev(this), VAL(msg))
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
    def eval(c: C) = {val (c2, i) = c.eval(id); val (c3, m) = msg.eval(c2); c3.send(i, m)}
  }
  case class GET[C <: Context[C], I <: Id[O], O](id: E[C, I]) extends E[C, O] {
    def eval(c: C) = { val (c2, i) = c.eval(id) ; c2.get(i) }
  }
  case class PREVIOUS[C <: Context[C], I <: Id[O], O](id: E[C, I]) extends E[C, O] {
    def eval(c: C) = { val (c2, i) = c.eval(id) ; c2.previous(i) }
  }
  case class GET_CONTEXT[C <: Context[C]]() extends E[C, C] {
    def eval(c: C) = { (c, c) }
  }
  case class SET_CONTEXT[C <: Context[C]](cc: E[C, C]) extends E[C, Unit] {
    def eval(c: C) = {val (_, c3) = c.eval(cc) ; (c3, ()) }
  }

  trait MsgSyntax[C <: Context[C], I <: Id[O], O, +R] extends Msg[C, I, O, R] {
    implicit def lift[A](a: A): E[A] = VAL(a)
    implicit class IdSyntax[O2](val id: Id[O2]) {
      def obj[X](f: O2 => X) = MAP(GET[C, Id[O2], O2](id), f)
      def old[X](f: O2 => X) = MAP(PREVIOUS[C, Id[O2], O2](id), f)
    }
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
      val vid = VAL[SContext, I](id)

      val (c2, pre_) = msg.preCondition(vid).eval(this)
      if (!pre_) sys.error("Pre-condition failed")
      else {
        val (_, app_) = msg.apply(vid).eval(c2)  // ignore any side effects
        val (c4, eff_) = msg.effect(vid).eval(SContext(this, c2.state + (id -> app_)))
        val (_, pst_) = msg.postCondition(vid).eval(SContext(this, c4.state))
        if (!pst_) sys.error("Post-condition failed")
        else (c4, eff_)
      }
    }
  }

  val contextLocal = new ThreadLocal[Any]
  val idLocal = new ThreadLocal[Any]

  trait LMsg[C <: Context[C], I <: Id[O], O, +R] extends Msg[C, I, O, R] {
    def preCondition2: Boolean
    def apply2: O
    def effect2: R
    def postCondition2: Boolean

    def preCondition: E[I] => E[Boolean] = i => s(i, preCondition2)
    def apply: E[I] => E[O] = i => s(i, apply2)
    def effect: E[I] => E[R] = i => s(i, effect2)
    def postCondition: E[I] => E[Boolean] = i => s(i, postCondition2)

    def lift[X](x: X): E[X] = VAL[C, X](x)

    def setContext(c: C): Unit = contextLocal.set(c)
    def getContext: C = contextLocal.get().asInstanceOf[C]

    def s[X](id: E[I], f: => X): E[X] = {
      try {
        for {
          i <- id
          c <- GET_CONTEXT[C]()
          _ <- lift(setContext(c))
          _ <- lift(idLocal.set(i))
          r <- lift {
            try f
            catch {
              case e: Exception => contextLocal.set(null) ; idLocal.set(null) ; throw e
            }
          }
          _ <- SET_CONTEXT(lift(getContext))
          _ <- lift(contextLocal.set(null))
          _ <- lift(idLocal.set(null))
        } yield r
      }
    }

    def obj_[O2](id: Id[O2]): O2 = obj(id)
    def old_[O2](id: Id[O2]): O2 = old(id)

    def obj[O2](id: Id[O2]): O2 = { val r = getContext.get(id) ; setContext(r._1) ; r._2 }
    def old[O2](id: Id[O2]): O2 = { val r = getContext.previous(id) ; setContext(r._1) ; r._2 }

    def send[I2 <: Id[O2], O2, R2](id: I2, msg: Msg[C, I2, O2, R2]): R2 = {
      val r = getContext.send(id, msg) ; setContext(r._1) ; r._2
    }

    def self: I = idLocal.get().asInstanceOf[I]
    def obj: O = obj(self)
    def old: O = old(self)
  }

  trait SMsg[C <: Context[C], I <: Id[O], O, +R] extends MsgSyntax[C, I, O, R] {
    type S = Self
    type P = Prev

    def preCondition2: S => E[Boolean]
    def apply2: S => E[O]
    def effect2: S => E[R]
    def postCondition2: P => E[Boolean]

    def obj2(id: E[I]): E[O] = GET[C, I, O](id)
    def old2(id: E[I]): E[O] = PREVIOUS[C, I, O](id)

    def preCondition: E[I] => E[Boolean] = i => {
      for {id <- i; obj <- obj2(i); pre <- preCondition2(SSelf(i, id, obj))} yield pre
    }
    def apply: E[I] => E[O] = i => {
      for {id <- i; obj <- obj2(i); app <- apply2(SSelf(i, id, obj))} yield app
    }
    def effect: E[I] => E[R] = i => {
      for {id <- i; obj <- obj2(i); eff <- effect2(SSelf(i, id, obj))} yield eff
    }
    def postCondition: E[I] => E[Boolean] = i => {
      for {id <- i; obj <- obj2(i); old <- old2(i); pst <- postCondition2(SPrev(i, id, obj, old))} yield pst
    }

    trait Self { def self: E[I]; def id: I; def obj: O }
    trait Prev extends Self { def old: O }

    case class SSelf(self: E[I], id: I, obj: O) extends Self
    case class SPrev(self: E[I], id: I, obj: O, old: O) extends Prev
  }

  val threadSelf = new ThreadLocal[Any]
  val threadPrev = new ThreadLocal[Any]

  trait TMsg[C <: Context[C], I <: Id[O], O, +R] extends SMsg[C, I, O, R] {
    def preCondition3: E[Boolean]
    def apply3: E[O]
    def effect3: E[R]
    def postCondition3: E[Boolean]

    def preCondition2 = s => self(s, preCondition3)
    def apply2 = s => self(s, apply3)
    def effect2 = s => self(s, effect3)
    def postCondition2 = p => prev(p, postCondition3)

    private def setSelf(s: S): Unit = threadSelf.set(s)
    private def getSelf: S = threadSelf.get().asInstanceOf[Self]
    private def setPrev(s: P): Unit = { setSelf(s); threadPrev.set(s) }
    private def getPrev: P = threadPrev.get().asInstanceOf[P]

    private def self[X](s: S, f: => E[X]) = try { setSelf(s); f } finally setSelf(null)
    private def prev[X](s: P, f: => E[X]) = try { setPrev(s); f } finally setPrev(null)

    def send[I2 <: Id[O2], O2, R2](id: I2, msg: Msg[C, I2, O2, R2]): E[R2] = SEND(VAL(id), VAL(msg))
    
    def obj: O = getSelf.obj
    def old: O = getPrev.old
    def self: I = getSelf.id
  }

  trait AMsg[C <: Context[C], I <: Id[O], O, +R] extends TMsg[C, I, O, R] {
    def pre: E[Boolean]
    def app: E[O]
    def eff: E[R]
    def pst: E[Boolean]

    def preCondition3 = pre
    def apply3 = app
    def effect3 = eff
    def postCondition3 = pst
  }

  trait UMsg[C <: Context[C], I <: Id[O], O, +R] extends LMsg[C, I, O, R] {
    def pre: Boolean
    def app: O
    def eff: R
    def pst: Boolean

    def preCondition2 = pre
    def apply2 = app
    def effect2 = eff
    def postCondition2 = pst

    implicit class IdSyntax[O2](val id: Id[O2]) {
      def obj: O2 = obj_(id)
      def old: O2 = old_(id)
    }
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

  case class Book[C <: Context[C]](from: AccountId = null, to: AccountId = null, amount: Double = 0) extends UMsg[C, TransferId, Transfer, Unit] {
    def pre = amount > 0.0
    def app = Transfer(from, to, amount)
    def eff = { send(from, Withdraw(amount)) ; send(to, Deposit(amount)) }
    def pst = from.obj.balance + to.obj.balance == from.old.balance + to.old.balance
  }
  
  case class Book2[C <: Context[C]](from: AccountId = null, to: AccountId = null, amount: Double = 0) extends AMsg[C, TransferId, Transfer, Unit] {
    def pre = amount > 0.0
    def app = Transfer(from, to, amount)
    def eff = { send(from, Withdraw(amount)) THEN send(to, Deposit(amount)) }
    def pst = from.obj(balance) + to.obj(balance) === from.old(balance) + to.old(balance)
  }

  def main(args: Array[String]): Unit = {
    val c = SContext()

    val a1 = AccountId("A1")
    val a2 = AccountId("A2")
    val t1 = TransferId(1)

    val e = c(a1) ! Open(50) THEN c(a2) ! Open(80) THEN c(t1) ! Book(a1, a2, 30)

    val r = e.eval(c)
    println("r: " + r._1.state)
  }
}
