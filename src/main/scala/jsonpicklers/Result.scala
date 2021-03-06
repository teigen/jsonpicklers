package jsonpicklers

object Result {
  case class Success[+A](value:A, location:Location) extends Result[A]{
    def isSuccess = true
  }
  case class Failure(msg:String, location:Location) extends Result[Nothing]{
    def isSuccess = false
  }
  trait FailProjection[+A]{
    def msg(what:String):Result[A]
  }
}

sealed trait Result[+A]{ result =>
  import Result._

  def location:Location

  def map[B](f:A => B) = this match {
    case Success(value, location) => Success(f(value), location)
    case f:Failure                => f
  }
  def flatMap[B](f:A => Result[B]) = this match {
    case Success(value, location) => f(value)
    case fail:Failure             => fail
  }
  def orElse[B >: A](other: => Result[B]) = this match {
    case f:Failure => other
    case n         => n
  }

  def fold[X](f:(String, Location) => X, s:(A, Location) => X):X = this match {
    case Success(value, l) => s(value, l)
    case Failure(msg, l)   => f(msg, l)
  }

  def either:Either[(String, Location), (A, Location)] = this match {
    case Success(value, location) => Right((value, location))
    case Failure(msg, location)   => Left((msg, location))
  }

  def isSuccess:Boolean

  def isFailure = !isSuccess

  def fail:FailProjection[A] = new FailProjection[A]{
    def msg(what: String): Result[A] = result match {
      case Failure(_, location) => Failure(what, location)
      case n => n
    }
  }

  def get:A = fold((m, l) => throw new NoSuchElementException("Failure("+l+", " + m + ", " + l.json +")"), (v, _) => v)
}
