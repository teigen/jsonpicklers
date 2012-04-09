import net.liftweb.json.JsonAST.JField

package object jsonpicklers {
  val *       = JsonProperty.all
  
  val string  = Primitive.string
  
  val boolean = Primitive.boolean
  
  val double  = Primitive.double
  
  val integer = Primitive.integer
  
  val bigint  = Primitive.bigint
  
  val NULL    = Primitive.NULL

  def array[A](a:JsonType[A]) = Primitive.array(a)

  def option[A](self:JsonProperty[A]) = Primitive.option[A](self)

  def property[A](name:String, self:JsonType[A]) = Combinators.property[A](name, self)

  def properties[A](predicate:JField => Boolean, self:JsonType[A]) = Combinators.properties(predicate, self)
} 