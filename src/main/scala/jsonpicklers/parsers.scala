package jsonpicklers

import Result.{Success, Failure}
import org.json4s.JsonAST._
import util.control.Exception.allCatch

object Parsers extends Parsers with FlattenTilde with Selectors

trait Parsers { parsers =>

  def success[A](value: A) =
    Parser{ location => Success(value, location) }

  def failure(msg: String) =
    Parser{ location => Failure(msg, location) }

  def acceptMatch[A](msg:String)(f:PartialFunction[JValue, A]) = Parser{ location =>
    f.lift(location.json).map(a => Success(a, location)).getOrElse(Failure("expected " + msg, location))
  }

  // json4s types
  val jvalue   = acceptMatch("JValue"){ case j:JValue => j }
  val jarray   = acceptMatch("JArray"){ case j:JArray => j }
  val jobject  = acceptMatch("JObject"){ case j:JObject => j }
  val jnothing = acceptMatch("JNothing"){ case j@JNothing => j }
  val jnull    = acceptMatch("JNull"){ case j@JNull => j }
  val jstring  = acceptMatch("JString"){ case j:JString => j }
  val jnumber  = acceptMatch("JNumber"){ case j:JNumber => j }
  val jdouble  = acceptMatch("JDouble"){ case j:JDouble => j }
  val jdecimal = acceptMatch("JDecimal"){ case j:JDecimal => j }
  val jint     = acceptMatch("JInt"){ case j:JInt => j }
  val jbool    = acceptMatch("JBool"){ case j:JBool => j }

  val NULL       = acceptMatch("null"){ case JNull => null }
  val nothing    = acceptMatch("Nothing"){ case JNothing => () }
  val int        = acceptMatch("Int"){ case JInt(v) if(v.isValidInt) => v.intValue() }
  val string     = acceptMatch("String"){ case JString(str) => str }
  val double     = acceptMatch("Double"){ case JDouble(d) => d }
  val bigint     = acceptMatch("Int"){ case JInt(v) => v }
  val bigdecimal = acceptMatch("Decimal"){ case JDecimal(v) => v }
  val boolean    = acceptMatch("Boolean"){ case JBool(b) => b }
  val long       = acceptMatch("Long"){ case JInt(v) if v.isValidLong => v.longValue() }
  val byte       = acceptMatch("Byte"){ case JInt(v) if v.isValidByte => v.byteValue() }
  val short      = acceptMatch("Short"){ case JInt(v) if v.isValidShort => v.shortValue() }
  val char       = acceptMatch("Char"){ case JInt(v) if v.isValidChar => v.charValue } |
                     string.filter(_.length == 1, "expected Char").map(_.charAt(0))

  def array[A](parser:Parser[A]) = parser.*

  def either[A, B](implicit left:Parser[A], right:Parser[B]):Parser[Either[A, B]] =
    left.map(Left(_)) | right.map(Right(_))

  def option[A](implicit parser:Parser[A]) = parser.?

  def not[A](parser:Parser[A]) = !parser

  def trying[A](f: => A) = Parser{ location =>
    try{ Success(f, location)} catch { case ex:Exception => Failure(ex.getMessage, location) }
  }

  def getOrFail[A](opt:Option[A], msg: => String) =
    opt.map(a => success(a)).getOrElse(failure(msg))

  def nullable[A](parser:Parser[A]):Parser[Option[A]] =
    NULL ^^^ None | parser.map(Some(_))

  implicit def stringParserOps(parser:Parser[String]) = new Parser.StringOps(parser)

  object lenient {
    private def orString[A](parser:Parser[A], orElse: String => A) = Parser{ location =>
      parser(location) match {
        case fail@Failure(msg, _) =>
          string.flatMap{ s => getOrFail(allCatch.opt(orElse), msg)}(location)
        case other => other
      }
    }

    val int        = orString(parsers.int,        _.toInt)
    val double     = orString(parsers.double,     _.toDouble)
    val bigint     = orString(parsers.bigint,     BigInt(_))
    val boolean    = orString(parsers.boolean,    _.toBoolean)
    val bigdecimal = orString(parsers.bigdecimal, BigDecimal(_))
    val long       = orString(parsers.long,       _.toLong)
    val byte       = orString(parsers.byte,       _.toByte)
    val short      = orString(parsers.short,      _.toShort)
    val string     = ( parsers.string
                     | parsers.double.map(_.toString)
                     | parsers.bigdecimal.map(_.toString())
                     | parsers.bigint.map(_.toString())
                     | parsers.boolean.map(_.toString) )

    def array[A](parser:Parser[A]) = parser.* | parser.map(List(_))
  }
}

object Parser {

  abstract class OrderingOps[A](parser:Parser[A]) {
    class Prop[B](name:String, prop:A => B)(implicit ordering:Ordering[B]){
      import ordering._

      def >  (rhs:B) = parser.filter(prop(_) >  rhs, "expected "+name+" > "  + rhs)
      def >= (rhs:B) = parser.filter(prop(_) >= rhs, "expected "+name+" >= " + rhs)
      def <  (rhs:B) = parser.filter(prop(_) <  rhs, "expected "+name+" < "  + rhs)
      def <= (rhs:B) = parser.filter(prop(_) <= rhs, "expected "+name+" <= " + rhs)
    }
  }

  class StringOps(parser:Parser[String]) extends OrderingOps(parser){
    object length extends Prop("length", _.length)
  }
}

case class Parser[+A](run:Location => Result[A]) extends (Location => Result[A]){

  def apply(location:Location) = run(location)

  def is[B >: A](value:B) = filter(_ == value, "expected " + value)

  def in[B >: A](values:B*) = filter(values.contains, "expected one of " + values.mkString(", "))

  def unary_! = Parser{ location =>
    run(location) match {
      case Success(_, _) => Failure("expected failure", location)
      case _             => Success((), location)
    }
  }

  def map[B](f:A => B):Parser[B] =
    Parser{ run(_).map(f) }

  def flatMap[B](f:A => Parser[B]): Parser[B] =
    Parser{ location => run(location).flatMap(f(_)(location)) }

  def ^^ [B] (f:A => B):Parser[B] =
    map(f)

  def ^^^ [B] (b: => B):Parser[B] =
    map(_ => b)

  def >> [B] (f:A => Parser[B]):Parser[B] =
    flatMap(f)

  def ~ [B] (rhs: => Parser[B]): Parser[(A, B)] = {
    lazy val r = rhs
    for{
      a <- this
      b <- r
    } yield (a, b)
  }

  def ~> [B] (rhs: => Parser[B]): Parser[B] =
    this ~ rhs ^^ { _._2 }

  def <~ [B] (rhs: => Parser[B]): Parser[A] =
    this ~ rhs ^^ { _._1 }

  def | [B >: A](rhs: => Parser[B]):Parser[B] = {
    lazy val r = rhs
    Parser{ location => apply(location).orElse(r(location)) }
  }

  def ? = map(Some(_)) | Parser{ location => Success(None, location) }

  def filter(f:A => Boolean, msg:String) = Parser{ location =>
    apply(location).flatMap(a => if(f(a)) Success(a, location) else Failure(msg, location))
  }

  def * = Parser{ location =>
    location.json match {
      case JArray(values) => (0 until values.size).foldRight[Result[List[A]]](Success(Nil, location)){
        case (index, lst) => for {
          u    <- run(location(index))
          tail <- lst
        } yield u :: tail
      }
      case _ => Failure("expected array", location)
    }
  }

  def + = *.filter(!_.isEmpty, "expected non-empty array")

  def :: (name:String) = Parser{ location => run(location(name)) }

  def :: (selector:Selector) = Parser{ location =>
    location.json match {
      case JObject(fields) =>
        fields.filter(field => selector.filter(field._1)).foldLeft[Result[Map[String, A]]](Success(Map.empty, location)){
          (map, field) => for{
            m <- map
            v <- run(location(field._1))
          } yield m + (field._1 -> v)
        }
      case _ => Failure("expected object", location)
    }
  }

  def :: (approximate:Approximate.Field) = Parser{ location => run(approximate(location)) }

  def ~:: (name:String)(implicit approximate:Approximate) = approximate(name) :: this

  def >  [B >: A](rhs:B)(implicit ordering:Ordering[B]) = filter(a => ordering.gt(a, rhs),   "expected value > "  + rhs)
  def >= [B >: A](rhs:B)(implicit ordering:Ordering[B]) = filter(a => ordering.gteq(a, rhs), "expected value >= " + rhs)
  def <  [B >: A](rhs:B)(implicit ordering:Ordering[B]) = filter(a => ordering.lt(a, rhs),   "expected value < "  + rhs)
  def <= [B >: A](rhs:B)(implicit ordering:Ordering[B]) = filter(a => ordering.lteq(a, rhs), "expected value <= " + rhs)
}

