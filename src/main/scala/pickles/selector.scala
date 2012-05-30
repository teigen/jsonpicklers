package pickles

import util.matching.Regex

case class Selector(filter:String => Boolean)

object Selector {

  val * = Selector(_ => true)

  implicit def filter(f:String => Boolean) =
    Selector(f)

  implicit def regex(r:Regex) =
    Selector(r.pattern.matcher(_).matches())
}