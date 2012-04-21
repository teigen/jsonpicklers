import net.liftweb.json.JsonAST.{JValue, JField}

package object jsonpicklers {
  val *       = JsonProperty.all
  
  val string  = JsonType.string
  
  val boolean = JsonType.boolean
  
  val double  = JsonType.double
  
  val integer = JsonType.integer
  
  val bigint  = JsonType.bigint
  
  val long    = JsonType.long
  
  val NULL    = JsonType.NULL

  def array[A](a:JsonTypeLike[A]) = JsonType.array[A](a.asType)

  def option[A, Like[_]](self:Optional[A, Like]) = self.?

  def property[A](name:String, json:JsonTypeLike[A]) = JsonProperty(name, json.asType)

  def properties[A](predicate:JField => Boolean, self:JsonTypeLike[A]) = JsonProperty.properties(predicate, self.asType)
} 