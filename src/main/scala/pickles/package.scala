package object pickles {
  val  * = Selector.*
  
  val string   = Json.string
  val int      = Json.int
  val bigint   = Json.bigint
  val boolean  = Json.boolean
  val NULL     = Json.NULL
  
  val syntax = Json.syntax
  def array[A](value:JsonValue[A]) = Json.array(value)
  def field[A](name:String, value:JsonValue[A]) = Json.field(name, value)
  def sequence[A, B](a:JsonObject[A], b:JsonObject[B]) = Json.sequence(a, b)
  def wrapper[A, B, Like[_] : Wrapper](like:Like[A])(w:A => B)(u:B => A) = Json.wrapper(like)(w)(u)
  def option[A, Like[_] : Optional](like:Like[A]) = Json.option(like)
  def or[T, A <: T : Reify, B <: T : Reify, Like[_] : Or](a:Like[A], b:Like[B]) = Json.or(a, b)
  def select[A](filter:String => Boolean, value:JsonValue[A]) = Json.select(filter, value)
  
  def wrap[A, B](w:A => B)(u:B => A) = new Wrappable[A, B] {
    def apply[Like[_] : Wrapper](like: Like[A]) = wrapper(like)(w)(u)
  }
  
  trait Wrappable[A, B]{
    def apply[Like[_]: Wrapper](like:Like[A]):Like[B]
  }
}