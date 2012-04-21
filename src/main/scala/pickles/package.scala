package object pickles {
  val  * = Selector.*
  
  val string   = Json.string
  val int      = Json.int
  val double   = Json.double
  val bigint   = Json.bigint
  val boolean  = Json.boolean
  val NULL     = Json.NULL
  
  val tilde   = Json.tilde
  val flatten = Json.flatten
  
  def array[A, Like[_]](value:Syntax[A, Like])(implicit ev:Like[A] => JsonValue[A]) = value.*
  def property[A](name:String, value:JsonValue[A]) = Json.property(name, value)
  def sequence[A, B](a:JsonObject[A], b:JsonObject[B]) = Json.sequence(a, b)
  def wrap[A, B](w:A => B)(u:B => A) = new Wrap(w, u)
  def option[A, Like[_] : Optional](value:Syntax[A, Like]) = value.?
  def getOrElse[A, Like[_] : Optional : Wrapper](value:Syntax[A, Like], orElse: => A) = value.?(orElse)
  def or[T, A <: T : Reify, B <: T : Reify, Like[_] : Or](a:Like[A], b:Like[B]) = Json.or(a, b)
  def select[A](filter:String => Boolean, value:JsonValue[A]) = Json.select(filter, value)
  
  class Wrap[A, B](a:A => B, b:B => A){
    def apply[Like[_] : Wrapper](like:Like[A]):Like[B] = Wrapper[Like].wrap(like)(a)(b)
  }
}