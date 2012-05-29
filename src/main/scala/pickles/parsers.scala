package pickles

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

object Parser {
  def apply[A](f:Location => Result[A]):Parser[A] = new Parser[A]{
    def apply(location: Location) = f(location)
  }
  
  def success[A](value: => A) = 
    Parser{ location => Success(value, location) }
  
  def failure(msg: => String) =
    Parser{ location => Failure(msg, location) }
}

trait Parser[+A] extends (Location => Result[A]){

  def map[B](f:A => B):Parser[B] = 
    Parser{ apply(_).map(f) }
  
  def flatMap[B](f:A => Parser[B]): Parser[B] = 
    Parser{ in => apply(in).flatMap(f(_)(in)) }
  
  def ^^ [B] (f:A => B) = map(f)
  def >> [B] (f:A => Parser[B]) = flatMap(f)
  
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
}