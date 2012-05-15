import net.liftweb.json.JsonAST._

package object pickles extends FlattenTilde {
  val  * = Selector.*

  val string  = JsonValue("string"){ case JString(s) => s }(JString(_))
  val int     = JsonValue("int"){ case JInt(i) => i.intValue() }(JInt(_))
  val double  = JsonValue("double"){ case JDouble(d) => d }(JDouble(_))
  val bigint  = JsonValue("bigint"){ case JInt(i) => i }(JInt(_))
  val boolean = JsonValue("boolean"){ case JBool(b) => b}(JBool(_))
  val NULL    = new JsonValue[Null]{
    def pickle(a: Null) = JNull
    def unpickle(location: Location) = location.json match {
      case JNull => Success(null, location)
      case _ => Failure("expected null", location)
    }
    def apply[B](value:B):JsonValue[B] = wrap(_ => value)(_ => null)
  }
  
  object tilde extends TildeSyntax
  
  def array[A](value:JsonValue[A]) = value.*
  def property[A](name:String, value:JsonValue[A]) = JsonProperty(name, value)
  def wrap[A, B](w:A => B)(u:B => A) = new Wrapper(w, u)
  def option[A, Like[_] : Optional](value:Like[A]):Like[Option[A]] = Optional[Like].optional(value)
  def getOrElse[A, Like[_] : Optional : Wrap](value:Like[A], orElse: => A):Like[A] = Wrap[Like].wrap(Optional[Like].optional(value))(_.getOrElse(orElse))(Some(_))
  def select[A](filter:String => Boolean, value:JsonValue[A]) = Selector.filter(filter) :: value
  def or[T, A <: T : Reify, B <: T : Reify, Like[_] : Or](a:Like[A], b:Like[B]):Like[T] = Or[Like].or(a, b)
  
  class Wrapper[A, B](a:A => B, b:B => A){
    def apply[Like[_] : Wrap](like:Like[A]):Like[B] = Wrap[Like].wrap(like)(a)(b)
  }
  
  def either[A : Reify, B : Reify, Like[_] : Or : Wrap](a:Like[A], b:Like[B]):Like[Either[A, B]] = {
    val left  = Wrap[Like].wrap(a)(Left(_))(_.a)
    val right = Wrap[Like].wrap(b)(Right(_))(_.b)
    or(left, right)
  }
  
  def unique[A](values:JsonValue[List[A]]) = values.filter(v => v.distinct == v, "expected all elements to be unique")
}