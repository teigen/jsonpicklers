package jsonpicklers

import annotation.implicitNotFound

@implicitNotFound("Don't know how to box ${A}")
trait Boxable[A <: AnyVal, B <: Object]{
  def box[Like[_]](a:JsonLike[A, Like]):Like[B]
}

// TODO instances
object Boxable {
  implicit val int = new Boxable[Int, java.lang.Integer]{
    def box[Like[_]](a: JsonLike[Int, Like]) = a.wrap(Int.box)(Int.unbox)
  }

  implicit val boolean = new Boxable[Boolean, java.lang.Boolean]{
    def box[Like[_]](a: JsonLike[Boolean, Like]) = a.wrap(Boolean.box)(Boolean.unbox)
  }
}
