package jsonpicklers

import Result.{Success, Failure}
import org.json4s.JsonAST._
import util.control.Exception.allCatch

object Parsers extends Parsers with FlattenTilde

trait Parsers { parsers =>
  val * = Selector.*

  def success[A](value: => A) =
    Parser{ location => Success(value, location) }

  def failure(msg: => String) =
    Parser{ location => Failure(msg, location) }

  // json4s types
  val jvalue   = Parser.value("JValue"){ case j:JValue => j }
  val jarray   = Parser.value("JArray"){ case j:JArray => j }
  val jobject  = Parser.value("JObject"){ case j:JObject => j }
  val jnothing = Parser.value("JNothing"){ case j@JNothing => j }
  val jnull    = Parser.value("JNull"){ case j@JNull => j }
  val jstring  = Parser.value("JString"){ case j:JString => j }
  val jnumber  = Parser.value("JNumber"){ case j:JNumber => j }
  val jdouble  = Parser.value("JDouble"){ case j:JDouble => j }
  val jdecimal = Parser.value("JDecimal"){ case j:JDecimal => j }
  val jint     = Parser.value("JInt"){ case j:JInt => j }
  val jbool    = Parser.value("JBool"){ case j:JBool => j }

  // regular types
  val NULL       = Parser.value("null"){ case JNull => null }
  val nothing    = Parser.value("Nothing"){ case JNothing => () }
  val int        = Parser.value("Int"){ case JInt(v) if(v.isValidInt) => v.intValue() }
  val string     = Parser.value("String"){ case JString(str) => str }
  val double     = Parser.value("Double"){ case JDouble(d) => d }
  val bigint     = Parser.value("Int"){ case JInt(v) => v }
  val bigdecimal = Parser.value("Decimal"){ case JDecimal(v) => v }
  val boolean    = Parser.value("Boolean"){ case JBool(b) => b }
  val long       = Parser.value("Long"){ case JInt(v) if v.isValidLong => v.longValue() }
  val byte       = Parser.value("Byte"){ case JInt(v) if v.isValidByte => v.byteValue() }
  val short      = Parser.value("Short"){ case JInt(v) if v.isValidShort => v.shortValue() }
  val char       = Parser.value("Char"){ case JInt(v) if v.isValidChar => v.charValue } |
                   string.filter(_.length == 1, "expected Char").map(_.charAt(0))

  def array[A](parser:Parser[A]) = Parser{ location =>
    location.json match {
      case JArray(values) => (0 until values.size).foldRight[Result[List[A]]](Success(Nil, location)){
        case (index, lst) => for {
          u    <- parser(location(index))
          tail <- lst
        } yield u :: tail
      }
      case _ => Failure("expected array", location)
    }
  }

  def trying[A](f: => A) = Parser{ location =>
    try{ Success(f, location)} catch { case ex:Exception => Failure(ex.getMessage, location) }
  }

  def getOrElse[A](opt:Option[A], msg: => String) =
    opt.map(a => success(a)).getOrElse(failure(msg))

  def option[A](parser:Parser[A]):Parser[Option[A]] =
    parser.?

  def either[A, B](left:Parser[A], right:Parser[B]):Parser[Either[A, B]] =
    left.map(Left(_)) | right.map(Right(_))

  implicit def stringParserOps(parser:Parser[String]) = new Parser.StringOps(parser)

  object lenient {
    private def orString[A](name:String, parser:Parser[A], orElse: String => A) =
      parser | string.flatMap{ s => getOrElse(allCatch.opt(orElse), "expected " + name)}

    val int        = orString("Int",     parsers.int,        _.toInt)
    val double     = orString("Double",  parsers.double,     _.toDouble)
    val bigint     = orString("Int",     parsers.bigint,     BigInt(_))
    val boolean    = orString("Boolean", parsers.boolean,    _.toBoolean)
    val bigdecimal = orString("Decimal", parsers.bigdecimal, BigDecimal(_))
    val long       = orString("Long",    parsers.long,       _.toLong)
    val byte       = orString("Byte",    parsers.byte,       _.toByte)
    val short      = orString("Short",   parsers.short,      _.toShort)

    def array[A](parser:Parser[A]) = parser.* | parser.map(List(_))
  }
}

object Parser {
  trait FailProjection[+A]{
    def msg(what:String):Parser[A]
  }

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

  def value[A](name:String)(f:PartialFunction[JValue, A]) = Parser{ location =>
    f.lift(location.json).map(a => Success(a, location)).getOrElse(Failure("expected "+name, location))
  }
}

case class Parser[+A](run:Location => Result[A]) extends (Location => Result[A]){

  def apply(location:Location) = run(location)

  def fail[B >: A]:Parser.FailProjection[B] = new Parser.FailProjection[B]{
    def msg(what: String): Parser[B] = Parser{ run(_).fail.msg(what) }
  }
  
  def map[B](f:A => B):Parser[B] = 
    Parser{ apply(_).map(f) }
  
  def flatMap[B](f:A => Parser[B]): Parser[B] = 
    Parser{ location => apply(location).flatMap(f(_)(location)) }
  
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
    Parser{ in => apply(in).orElse(r(in)) }
  }
  
  def ? :Parser[Option[A]] =
    Parsers.NULL ^^^ None | map(Some(_))

  def filter(f:A => Boolean, msg:String) =
    Parser{ location => 
      apply(location).flatMap(a => if(f(a)) Success(a, location) else Failure(msg, location)) 
    }

  def * = Parsers.array(this)

  def :: (name:String):Parser[A] = new Parser[A](location => apply(location(name))){
    override def ? = (name :: Parsers.nothing) ^^^ None | map(Some(_))
  }

  def :: (selector:Selector) = Parser{ location =>
    location.json match {
      case JObject(fields) =>
        fields.filter(field => selector.filter(field._1)).foldLeft[Result[Map[String, A]]](Success(Map.empty, location)){
          (map, field) => for{
            m <- map
            v <- apply(location(field._1))
          } yield m + (field._1 -> v)
        }
      case _ => Failure("expected object", location)
    }
  }

  def >  [B >: A](rhs:B)(implicit ordering:Ordering[B]) = filter(a => ordering.gt(a, rhs),   "expected value > "  + rhs)
  def >= [B >: A](rhs:B)(implicit ordering:Ordering[B]) = filter(a => ordering.gteq(a, rhs), "expected value >= " + rhs)
  def <  [B >: A](rhs:B)(implicit ordering:Ordering[B]) = filter(a => ordering.lt(a, rhs),   "expected value < "  + rhs)
  def <= [B >: A](rhs:B)(implicit ordering:Ordering[B]) = filter(a => ordering.lteq(a, rhs), "expected value <= " + rhs)
}