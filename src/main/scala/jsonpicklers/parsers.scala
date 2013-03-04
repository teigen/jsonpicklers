package jsonpicklers

import org.json4s.JsonAST._
import java.text.{SimpleDateFormat}

object Parsers extends Parsers with FlattenTilde

trait Parsers {

  val * = Selector.*

  def expect[A](name:String)(f:PartialFunction[JValue, A]) = Parser{ location =>
    f.lift(location.json).map(a => Success(a, location)).getOrElse(Failure("expected "+name, location))
  }
  
  def success[A](value: => A) =
    Parser{ location => Success(value, location) }

  def failure(msg: => String) =
    Parser{ location => Failure(msg, location) }

  def NULL = expect("null"){ case JNull => null }

  def nothing = expect("Nothing"){ case JNothing => () }
  
  def int = expect("Int"){ case JInt(v) if(v.isValidInt) => v.intValue() }
  
  def string = expect("String"){ case JString(str) => str }
  
  def double = expect("Double"){ case JDouble(d) => d }
  
  def bigint = expect("Int"){ case JInt(v) => v }
  
  def boolean = expect("Boolean"){ case JBool(b) => b }

  def date(format: => SimpleDateFormat) = string.flatMap{
    s => Parser{ location => try{ Success(format.parse(s), location) } catch { case ex:Exception => Failure("expected date ("+ format.toPattern +")", location)} }
  }

  def date(format:String):Parser[java.util.Date] = date(new SimpleDateFormat(format))

  def trying[A](f: => A) = Parser{ location =>
    try{ Success(f, location)} catch { case ex:Exception => Failure(ex.getMessage, location) }
  }

  def option[A](parser:Parser[A]):Parser[Option[A]] =
    parser.?

  def either[A, B](left:Parser[A], right:Parser[B]):Parser[Either[A, B]] =
    left.map(Left(_)) | right.map(Right(_))

  class StringParserOps(parser:Parser[String]){
    class Prop[A](name:String, prop:String => A)(implicit ordering:Ordering[A]){
      import ordering._

      def >  (rhs:A) = parser.filter(prop(_) > rhs,  "expected "+name+" > "  + rhs)
      def >= (rhs:A) = parser.filter(prop(_) >= rhs, "expected "+name+" >= " + rhs)
      def <  (rhs:A) = parser.filter(prop(_) < rhs,  "expected "+name+" < "  + rhs)
      def <= (rhs:A) = parser.filter(prop(_) <= rhs, "expected "+name+" <= " + rhs)
    }

    object length extends Prop("length", _.length)
  }

  implicit def stringParserOps(parser:Parser[String]) = new StringParserOps(parser)
}

case class Parser[+A](run:Location => Result[A]) extends (Location => Result[A]){
  
  def apply(location:Location) = run(location)
  
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
    map(Some(_)) | Parsers.success(None)
  
  def filter(f:A => Boolean, msg:String) =
    Parser{ location => 
      apply(location).flatMap(a => if(f(a)) Success(a, location) else Failure(msg, location)) 
    }

  def * = Parser{ location =>
    location.json match {
      case JArray(values) => (0 until values.size).foldRight[Result[List[A]]](Success(Nil, location)){
        case (index, lst) => for {
          u    <- apply(location(index))
          tail <- lst
        } yield u :: tail
      }
      case _ => Failure("expected array", location)
    }
  }

  def :: (name:String) = Parser{ location => apply(location(name)) }

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