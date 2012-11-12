package examples

import jsonpicklers._
import Picklers._
import org.scalatest.FunSuite

class Polymorphic extends FunSuite {
  object Animal {
    val json:JsonObject[Animal] = Dog.json | Fish.json
  }
  sealed trait Animal

  object Dog {
    val json = wrap(apply)(unapply(_).get){
      "name" :: string
    }
  }
  case class Dog(name: String) extends Animal

  object Fish {
    val json = wrap(apply)(unapply(_).get){
      "weight" :: double
    }
  }
  case class Fish(weight: Double) extends Animal

  val animals = "animals" :: array(Animal.json)

  test("pickle/unpickle"){
    val a             = List(Dog("Snoopy"), Fish(2.5))
    val pickled       = animals.pickle(a)
    val Success(v, _) = animals.unpickle(pickled)
    assert(a === v)
  }
}
