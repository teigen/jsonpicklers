package jsonpicklers

import annotation.implicitNotFound
import net.liftweb.json.JsonAST.JValue

@implicitNotFound("Don't know how to box ${A}")
trait Boxable[A <: AnyVal, B <: Object]{
  def box[Pickle <: JValue, Like[_]](a:JsonLike[A, Pickle, Like]):Like[B]
}

// TODO instances
object Boxable {
  implicit val int = new Boxable[Int, java.lang.Integer]{
    def box[Pickle <: JValue, Like[_]](a: JsonLike[Int, Pickle, Like]) = a.wrap(Int.box)(Int.unbox)
  }

  implicit val boolean = new Boxable[Boolean, java.lang.Boolean]{
    def box[Pickle <: JValue, Like[_]](a: JsonLike[Boolean, Pickle, Like]) = a.wrap(Boolean.box)(Boolean.unbox)
  }
}
