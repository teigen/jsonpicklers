package jsonpicklers

import net.liftweb.json.JsonAST.{JObject, JValue}

// TODO clean up + model pickle/unpickle as typeclass

object CoVariantJsonType {
  implicit def covariantJsonType[A : Reify](jsonType:JsonType[A]):CoVariantJsonType[A] = new CoVariantJsonType[A](jsonType, implicitly[Reify[A]])
}

class CoVariantJsonType[+A](a:JsonType[A], reify:Reify[A]){
  private [jsonpicklers] def unsafePickle[B >: A](value:B):Option[JValue] = reify.reify.lift(value).map(unsafe.pickle)
  private [jsonpicklers] def unsafe[B >: A] = a.asInstanceOf[JsonType[B]]

  def or[B >: A](b:CoVariantJsonType[B]) = JsonType[B](value => (unsafePickle(value) orElse b.unsafePickle(value)).get, (location, json) => unsafe.unpickle(location, json) orElse b.unsafe.unpickle(location, json))
}

object CoVariantJsonObject {
  implicit def covariantJsonObject[A : Reify](jsonObject:JsonObject[A]):CoVariantJsonObject[A] = new CoVariantJsonObject[A](jsonObject, implicitly[Reify[A]])
}

class CoVariantJsonObject[+A](a:JsonObject[A], reify:Reify[A]){
  private [jsonpicklers] def unsafePickle[B >: A](value:B):Option[JObject] = reify.reify.lift(value).map(unsafe.pickle)
  private [jsonpicklers] def unsafe[B >: A] = a.asInstanceOf[JsonObject[B]]

  def or[B >: A](b:CoVariantJsonObject[B]) = JsonObject[B](value => (unsafePickle(value) orElse b.unsafePickle(value)).get, (location, json) => unsafe.unpickle(location, json) orElse b.unsafe.unpickle(location, json))
}