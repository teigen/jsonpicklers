package jsonpicklers

import annotation.implicitNotFound
import net.liftweb.json.JsonAST.JValue

@implicitNotFound("Don't know how to wrap ${A} in ${B}")
trait Wrap[A, B] { wrapper =>
  def wrap(value:A):B
  def unwrap(value:B):A
  def apply[Pickle <: JValue, Like[_]](like:JsonLike[A, Pickle, Like]) = like as this
}

object Wrap{
  def apply[A, B](w:A => B)(u:B => A):Wrap[A, B] = new Wrap[A, B]{
    def wrap(value: A)   = w(value)
    def unwrap(value: B) = u(value)
  }

  implicit val defaultBoolean = Wrap[Null, Boolean](_ => false)(_ => null)

  implicit def nullValue[A](a:A) = Wrap[Null, A](_ => a)(_ => null)
}
