package jsonpicklers

import org.json4s.JsonAST._

object Picklers extends Picklers with FlattenTilde with Selectors {
  lazy val parsers = Parsers
}

trait Picklers {

  def parsers:Parsers

  private def simple[A](parser:Parser[A], pickle:A => JValue) =
    Pickler[A](parser, value => Some(pickle(value)), JNothing)

  private def json4s[A <: JValue](parser:Parser[A]) = simple[A](parser, identity)

  //json4s types
  implicit val jvalue   = json4s(parsers.jvalue)
  implicit val jarray   = json4s(parsers.jarray)
  implicit val jobject  = json4s(parsers.jobject)
  implicit val jnothing = json4s(parsers.jnothing)
  implicit val jnull    = json4s(parsers.jnull)
  implicit val jstring  = json4s(parsers.jstring)
  implicit val jnumber  = json4s(parsers.jnumber)
  implicit val jdouble  = json4s(parsers.jdouble)
  implicit val jdecimal = json4s(parsers.jdecimal)
  implicit val jint     = json4s(parsers.jint)
  implicit val jbool    = json4s(parsers.jbool)

  val NULL       = simple[Null](parsers.NULL, _ => JNull)
  val nothing    = simple[Unit](parsers.nothing, _ => JNothing)

  val int        = simple[Int](parsers.int, JInt(_))
  val string     = simple[String](parsers.string, JString)
  val double     = simple[Double](parsers.double, JDouble)
  val bigint     = simple[BigInt](parsers.bigint, JInt)
  val bigdecimal = simple[BigDecimal](parsers.bigdecimal, JDecimal)
  val boolean    = simple[Boolean](parsers.boolean, JBool)
  val long       = simple[Long](parsers.long, JInt(_))
  val byte       = simple[Byte](parsers.byte, JInt(_))
  val short      = simple[Short](parsers.short, JInt(_))
  val char       = simple[Char](parsers.char, value => JInt(value))


  def array[A](pickler:Pickler[A]) = pickler.*

  def option[A](pickler:Pickler[A]) = pickler.?

  def nullable[A](pickler:Pickler[A]) = Pickler[Option[A]](
    parsers.nullable(pickler.unpickle),
    _.map(pickler.pickle).getOrElse(Some(JNull)), pickler.empty)

  def either[A : Reify, B : Reify](left:Pickler[A], right:Pickler[B]):Pickler[Either[A, B]] =
    left.xmap(Left(_))(_.a) | right.xmap(Right(_))(_.b)

  object xmap {
    def apply[A, B](map:A => B)(contramap:B => A) = new XMap[A, B](map, contramap)
  }

  object xflatMap {
    def apply[A, B](flatMap:A => Parser[B])(contraflatMap:B => Option[A]) = new XFlatMap[A, B](flatMap, contraflatMap)
  }
}

object Pickler {
  def apply[A](unpickle:Parser[A], pickle:A => Option[JValue], empty:JValue):Pickler[A] =
    new Value[A](unpickle, pickle, empty)

  class Value[A](val unpickle:Parser[A], val pickle:A => Option[JValue], val empty:JValue) extends Pickler[A]

  class Const[A](val const:A, pickler:Pickler[A]) extends Pickler[A]{
    def unpickle: Parser[A] = pickler.unpickle
    def pickle: (A) => Option[JValue] = pickler.pickle
    def empty: JValue = pickler.empty

    def ~> [B](next:Pickler[B]) = (pickler ~ next).xmap(_._2)(b => (const, b))
  }
}

trait Pickler[A] {
  def unpickle:Parser[A]
  def pickle:A => Option[JValue]
  def empty:JValue

  private def pickler[B](u:Parser[B], p:B => Option[JValue]) =
    Pickler(u, p, empty)

  def :: (name:String) = Pickler[A](
    name :: unpickle,
    pickle(_).map{
      case JNothing => JObject(Nil)
      case value    => JObject(name -> value)
    },
    JObject(Nil))

  def :: (selector:Selector) = Pickler[Map[String, A]](
    selector :: unpickle,
    _.foldRight[Option[List[JField]]](Some(Nil)){
      case ((name, a), acc) => for {
        fields <- acc
        pa <- pickle(a)
      } yield (name, pa) :: fields
    }.map(JObject(_)),
    JObject(Nil))

  def ~ [B](next:Pickler[B]) = pickler[(A, B)](
    unpickle ~ next.unpickle,
    { case (a, b) =>
      for {
        pa <- pickle(a)
        pb <- next.pickle(b)
      } yield pa merge pb
    })

  def filter(predicate:A => Boolean, msg:String) = pickler[A](
    unpickle.filter(predicate, msg),
    a => if(predicate(a)) pickle(a) else None)

  def ? = pickler[Option[A]](
    Parsers.option(unpickle),
    _.map(pickle).getOrElse(Some(empty)))

  def ? (orElse:A):Pickler[A] = ?.getOrElse(orElse)

  def ?? (orElse:A):Pickler[A] = ?.getOrElseIgnoreDefault(orElse)

  def * = Pickler[List[A]](
    Parsers.array(unpickle),
    _.foldRight[Option[List[JValue]]](Some(Nil)){ (a, acc) =>
      for {
        list <- acc
        pa <- pickle(a)
      } yield pa :: list
    }.map(JArray),
    JNothing)

  def + = *.filter(!_.isEmpty, "expected non-empty array")

  def >  [B >: A](rhs:B)(implicit ordering:Ordering[B]) = filter(a => ordering.gt(a, rhs),   "expected value > "  + rhs)
  def >= [B >: A](rhs:B)(implicit ordering:Ordering[B]) = filter(a => ordering.gteq(a, rhs), "expected value >= " + rhs)
  def <  [B >: A](rhs:B)(implicit ordering:Ordering[B]) = filter(a => ordering.lt(a, rhs),   "expected value < "  + rhs)
  def <= [B >: A](rhs:B)(implicit ordering:Ordering[B]) = filter(a => ordering.lteq(a, rhs), "expected value <= " + rhs)

  def xmap[B](map:A => B)(comap:B => A) = pickler[B](
    unpickle.map(map),
    comap andThen pickle)

  def ^^ [B](x:XMap[A, B]) = xmap(x.map)(x.contramap)

  def xflatMap[B](flatMap:A => Parser[B])(contraflatMap:B => Option[A]) = pickler[B](
    unpickle.flatMap(flatMap),
    contraflatMap(_).flatMap(pickle))

  def >> [B](x:XFlatMap[A, B]) = xflatMap(x.flatMap)(x.contraflatMap)

  def getOrElse[B](orElse:B)(implicit ev1:A =:= Option[B], ev2:Option[B] =:= A) = xmap(_.getOrElse(orElse))(Some(_))

  def getOrElseIgnoreDefault[B](orElse:B)(implicit ev1:A =:= Option[B], ev2:Option[B] =:= A) = xmap(_.getOrElse(orElse))(value => if(value == orElse) None else Some(value))

  def apply(const:A) = new Pickler.Const(const, this.filter(_ == const, "expected value == " + const))

  def apply(values:A*) = filter(values.contains, "expected one of " + values.mkString("(", ",", ")"))

  def <~ [B](next:Pickler.Const[B]) = (this ~ next).xmap(_._1)(a => (a, next.const))

  def | [T >: A,  B <: T](next:Pickler[B])(implicit ra:Reify[A], rb:Reify[B]) = pickler[T](
    (unpickle:Parser[T]) | next.unpickle,
    t => ra.reify(t).flatMap(pickle) orElse rb.reify(t).flatMap(next.pickle))
}

class XMap[A, B](val map:A => B, val contramap:B => A){
  def apply(pickler:Pickler[A]) = pickler ^^ this
}

class XFlatMap[A, B](val flatMap:A => Parser[B], val contraflatMap:B => Option[A]){
  def apply(pickler:Pickler[A]) = pickler >> this
}