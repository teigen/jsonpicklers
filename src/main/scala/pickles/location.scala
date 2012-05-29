package pickles

import net.liftweb.json.JsonAST.JValue

object Location {
  implicit def root(json:JValue) = Root(json)
}

sealed trait Location {
  def json:JValue

  override def toString = this match {
    case Root(_) => ""
    case ArrayLocation(_, index, parent) => parent.toString + "["+index+"]"
    case FieldLocation(_, name, Root(_)) => name
    case FieldLocation(_, name, parent)  => parent.toString+"."+name
  }
  def apply(name:String) = FieldLocation(json \ name, name, this)
  def apply(index:Int)   = ArrayLocation(json(index), index, this)
}

case class Root(json:JValue) extends Location
case class FieldLocation(json:JValue, name:String, parent:Location) extends Location
case class ArrayLocation(json:JValue, index:Int, parent:Location) extends Location