package jsonpicklers

sealed trait Location {
  override def toString = this match {
    case Root => ""
    case ArrayLocation(index, parent) => parent.toString + "["+index+"]"
    case FieldLocation(name, Root)    => name
    case FieldLocation(name, parent)  => parent.toString+"."+name
  }
  def apply(name:String) = FieldLocation(name, this)
  def apply(index:Int)   = ArrayLocation(index, this)
}

object Root extends Location
case class FieldLocation(name:String, parent:Location) extends Location
case class ArrayLocation(index:Int, parent:Location) extends Location
