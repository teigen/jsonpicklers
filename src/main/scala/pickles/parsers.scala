package pickles

import net.liftweb.json.JsonAST._

trait Result[+A]{
  def map[B](f:A => B) = this match {
    case Success(value, location) => Success(f(value), location)
    case f:Failure                => f
  }
  def flatMap[B](f:A => Result[B]) = this match {
    case Success(value, location) => f(value)
    case f:Failure                => f
  }
  def orElse[B >: A](other: => Result[B]) = this match {
    case f:Failure => other
    case n         => n
  }

  def isFailure = this match {
    case _:Failure => true
    case _         => false
  }

  def isSuccess = !isFailure
  
  def get:A = this match {
    case Success(value, _) => value
    case failure => sys.error(failure.toString)              
  }
}
case class Success[+A](value:A, location:Location) extends Result[A]
case class Failure(msg:String, location:Location) extends Result[Nothing]{
  override def toString = "Failure("+location+", "+msg + ", " + location.json+")"
}

object Parsers {
  
  def expect[A](name:String)(f:PartialFunction[JValue, A]) = Parser{ location =>
    f.lift(location.json).map(a => Success(a, location)).getOrElse(Failure("expected "+name, location))
  }
  
  def success[A](value: => A) =
    Parser{ location => Success(value, location) }

  def failure(msg: => String) =
    Parser{ location => Failure(msg, location) }

  def NULL = expect("null"){
    case JNull => null
  }

  def nothing = expect("Nothing"){
    case JNothing => ()
  }
  
  def int = expect("Int"){
    case JInt(v) if(v.isValidInt) => v.intValue()
  }
  
  def string = expect("String"){
    case JString(str) => str
  }
  
  def double = expect("Double"){
    case JDouble(d) => d
  }
  
  def bigint = expect("Int"){
    case JInt(v) => v
  }
  
  def boolean = expect("Boolean"){
    case JBool(b) => b
  }
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
  
  def ~ [B] (rhs: => Parser[B]): Parser[A ~ B] = {
    lazy val r = rhs
    for{
      a <- this
      b <- r
    } yield new ~(a, b)
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
        fields.filter(field => selector.filter(field.name)).foldLeft[Result[Map[String, A]]](Success(Map.empty, location)){
          (map, field) => for{
            m <- map
            v <- apply(location(field.name))
          } yield m + (field.name -> v)
        }
      case _ => Failure("expected object", location)
    }
  }
}