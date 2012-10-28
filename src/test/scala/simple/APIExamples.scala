package simple

import jsonpicklers.Picklers._
import java.net.URL

object Hello {
  val json = wrap(apply)(unapply(_).get){
    "message" :: string
  }
}
case class Hello(message:String)

object Sizes {
  val json = wrap(apply)(unapply(_).get){
    ("sizes" :: ( * :: Size.json))
  }
}
case class Sizes(shapes:Map[String,Size])

object Size {
  val json = wrap(apply)(unapply(_).get){
    ("x" :: int) ~
    ("y" :: int)
  }
}
case class Size(x:Int, y:Int)
