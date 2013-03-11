package jsonpicklers

import Result.{Success, Failure}
import org.json4s.JsonAST._
import util.control.Exception.allCatch

object Parsers extends Parsers with FlattenTilde

trait Parsers { parsers =>
  val * = Selector.*

  def success[A](value: A) =
    Parser{ location => Success(value, location) }

  def failure(msg: String) =
    Parser{ location => Failure(msg, location) }

  def acceptMatch[A](msg:String)(f:PartialFunction[JValue, A]) = Parser{ location =>
    f.lift(location.json).map(a => Success(a, location)).getOrElse(Failure("expected " + msg, location))
  }

  // json4s types
  implicit val jvalue   = acceptMatch("JValue"){ case j:JValue => j }
  implicit val jarray   = acceptMatch("JArray"){ case j:JArray => j }
  implicit val jobject  = acceptMatch("JObject"){ case j:JObject => j }
  implicit val jnothing = acceptMatch("JNothing"){ case j@JNothing => j }
  implicit val jnull    = acceptMatch("JNull"){ case j@JNull => j }
  implicit val jstring  = acceptMatch("JString"){ case j:JString => j }
  implicit val jnumber  = acceptMatch("JNumber"){ case j:JNumber => j }
  implicit val jdouble  = acceptMatch("JDouble"){ case j:JDouble => j }
  implicit val jdecimal = acceptMatch("JDecimal"){ case j:JDecimal => j }
  implicit val jint     = acceptMatch("JInt"){ case j:JInt => j }
  implicit val jbool    = acceptMatch("JBool"){ case j:JBool => j }

  // null and nothing are not implicit - we NEVER want to infer them implicitly
  val NULL       = acceptMatch("null"){ case JNull => null }
  val nothing    = acceptMatch("Nothing"){ case JNothing => () }
  // regular types
  implicit val int        = acceptMatch("Int"){ case JInt(v) if(v.isValidInt) => v.intValue() }
  implicit val string     = acceptMatch("String"){ case JString(str) => str }
  implicit val double     = acceptMatch("Double"){ case JDouble(d) => d }
  implicit val bigint     = acceptMatch("Int"){ case JInt(v) => v }
  implicit val bigdecimal = acceptMatch("Decimal"){ case JDecimal(v) => v }
  implicit val boolean    = acceptMatch("Boolean"){ case JBool(b) => b }
  implicit val long       = acceptMatch("Long"){ case JInt(v) if v.isValidLong => v.longValue() }
  implicit val byte       = acceptMatch("Byte"){ case JInt(v) if v.isValidByte => v.byteValue() }
  implicit val short      = acceptMatch("Short"){ case JInt(v) if v.isValidShort => v.shortValue() }
  implicit val char       = acceptMatch("Char"){ case JInt(v) if v.isValidChar => v.charValue } |
                            string.filter(_.length == 1, "expected Char").map(_.charAt(0))

  implicit def array[A](implicit parser:Parser[A]) = Parser{ location =>
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

  implicit def either[A, B](implicit left:Parser[A], right:Parser[B]):Parser[Either[A, B]] =
    left.map(Left(_)) | right.map(Right(_))

  implicit def option[A](implicit parser:Parser[A]):Parser[Option[A]] =
    Parsers.nothing ^^^ None | parser.map(Some(_))

  implicit def accept[A](value:A)(implicit parser:Parser[A]) = parser.filter(_ == value, "expected " + value)

  def field[A](name:String, parser:Parser[A]) = Parser{ location => parser(location(name)) }

  def field[A](approximate:Approximate.Field, parser:Parser[A]) = Parser{ location => parser(approximate(location)) }

  def fields[A](selector:Selector, parser:Parser[A]) = Parser{ location =>
    location.json match {
      case JObject(fields) =>
        fields.filter(field => selector.filter(field._1)).foldLeft[Result[Map[String, A]]](Success(Map.empty, location)){
          (map, field) => for{
            m <- map
            v <- parser(location(field._1))
          } yield m + (field._1 -> v)
        }
      case _ => Failure("expected object", location)
    }
  }

  def not[A](p: => Parser[A]) = Parser{ location =>
    p(location) match {
      case Success(_, _) => Failure("expected failure", location)
      case _             => Success((), location)
    }
  }

  def trying[A](f: => A) = Parser{ location =>
    try{ Success(f, location)} catch { case ex:Exception => Failure(ex.getMessage, location) }
  }

  def getOrFail[A](opt:Option[A], msg: => String) =
    opt.map(a => success(a)).getOrElse(failure(msg))

  def nullable[A](parser:Parser[A]):Parser[Option[A]] =
    Parsers.NULL ^^^ None | parser.map(Some(_))

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

object Approximate {

  case class Field(name:String, approximate:Approximate){
    def apply(location:Location) = location.json match {
      case JObject(fields) =>
        val distances = fields.flatMap{ case JField(fname, _) =>
          approximate.distance(fname, name).map( _ -> fname)
        }
        distances.sortWith(_._1 < _._1) match {
          case (distance, field) :: tail if tail.forall(_._1 > distance) =>
            location(field)
          case _ => FieldLocation(JNothing, name, location)
        }
      case _ => FieldLocation(JNothing, name, location)
    }
  }

  object Exact extends Approximate {
    def distance(goal: String, input: String): Option[Int] =
      if(goal == input) Some(0) else None
  }

  case class IgnoreCase(score:Int) extends Approximate {
    def distance(goal: String, input: String): Option[Int] =
      if(goal.equalsIgnoreCase(input)) Some(score) else None
  }

  case class Levenshtein(limit:Int) extends Approximate {
    def distance(s1: String, s2: String): Option[Int] = {
      val l1 = s1.length
      val l2 = s2.length

      def min(a:Int, b:Int, c:Int) = math.min(a, math.min(b, c))

      val d = Array.ofDim[Int](l1 + 1, l2 + 1)

      for(i <- 0 to l1) d(i)(0) = i
      for(j <- 0 to l2) d(0)(j) = j

      for(i <- 1 to l1; j <- 1 to l2){
        val cost = if(s1(i-1) == s2(j-1)) 0 else 1
        d(i)(j) = min(
          d(i-1)(j  ) + 1,
          d(i  )(j-1) + 1,
          d(i-1)(j-1) + cost
        )
      }
      val result = d(l1)(l2)
      if(result <= limit)
        Some(result)
      else None
    }
  }
}

trait Approximate {
  def distance(goal:String, input:String):Option[Int]
  def apply(name:String) = Approximate.Field(name, this)
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

  def unary_! = Parsers.not(this)

  def withFailureMessage(msg:String) = Parser{ location =>
    apply(location) match {
      case Failure(_, next) => Failure(msg, next)
      case other => other
    }
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
    Parser{ location => apply(location).orElse(r(location)) }
  }
  
  def ? = Parsers.option(this)

  def filter(f:A => Boolean, msg:String) = Parser{ location => 
    apply(location).flatMap(a => if(f(a)) Success(a, location) else Failure(msg, location)) 
  }

  def * = Parsers.array(this)

  def + = *.filter(!_.isEmpty, "expected non-empty array")

  def :: (name:String) = Parsers.field(name, this)

  def :: (selector:Selector) = Parsers.fields(selector, this)

  def :: (approximate:Approximate.Field) = Parsers.field(approximate, this)

  def ~:: (name:String)(implicit approximate:Approximate) = approximate(name) :: this

  def >  [B >: A](rhs:B)(implicit ordering:Ordering[B]) = filter(a => ordering.gt(a, rhs),   "expected value > "  + rhs)
  def >= [B >: A](rhs:B)(implicit ordering:Ordering[B]) = filter(a => ordering.gteq(a, rhs), "expected value >= " + rhs)
  def <  [B >: A](rhs:B)(implicit ordering:Ordering[B]) = filter(a => ordering.lt(a, rhs),   "expected value < "  + rhs)
  def <= [B >: A](rhs:B)(implicit ordering:Ordering[B]) = filter(a => ordering.lteq(a, rhs), "expected value <= " + rhs)
}