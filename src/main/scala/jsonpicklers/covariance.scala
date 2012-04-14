package jsonpicklers

import net.liftweb.json.JsonAST.{JObject, JValue}

object CoVariantJsonType {
  implicit def covariantJsonType[A : Reify](jsonType:JsonTypeLike[A]):CoVariantJsonType[A] = new CoVariantJsonType[A](jsonType.asType, implicitly[Reify[A]])
}

class CoVariantJsonType[+A](a:JsonType[A], reify:Reify[A]){
  private [jsonpicklers] def unsafePickle[B >: A](value:B):Option[JValue] = reify.reify.lift(value).map(unsafe.pickle)
  private [jsonpicklers] def unsafe[B >: A] = a.asInstanceOf[JsonType[B]]

  def or[B >: A](b:CoVariantJsonType[B]) = JsonType[B](value => (unsafePickle(value) orElse b.unsafePickle(value)).get, (location, json) => unsafe.unpickle(location, json) orElse b.unsafe.unpickle(location, json))
}

object CoVariantJsonObject {
  implicit def covariantJsonObject[A : Reify](jsonObject:JsonObjectLike[A]):CoVariantJsonObject[A] = new CoVariantJsonObject[A](jsonObject.asObject, implicitly[Reify[A]])
}

class CoVariantJsonObject[+A](a:JsonObject[A], reify:Reify[A]){
  private [jsonpicklers] def unsafePickle[B >: A](value:B):Option[JObject] = reify.reify.lift(value).map(unsafe.pickle)
  private [jsonpicklers] def unsafe[B >: A] = a.asInstanceOf[JsonObject[B]]
  // nei, nei... identity som mod blir ikke riktig
  def or[B >: A](b:CoVariantJsonObject[B]) = JsonObject[B](identity, value => (unsafePickle(value) orElse b.unsafePickle(value)).get, (location, json) => unsafe.unpickle(location, json) orElse b.unsafe.unpickle(location, json))
}