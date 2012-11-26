package jsonpicklers

import Picklers._

import org.scalatest.PropSpec
import org.json4s.JsonAST._

class EnumerableTest extends PropSpec {
  property("string literal / positive") {

    val field = "a" :: string("b")

    val json = JObject("a" -> JString("b"))

    val unpickled = field.unpickle(json).get

    assert(unpickled === "b")

    assert(field.pickle("b") == json)
  }

  property("string literal / negative") {
    val field = "a" :: string("b")

    val json = JObject("a" -> JString("a"))

    val unpickled = field.unpickle(json)

    unpickled match {
      case s@Success(_, _) => fail(s.toString)
      case Failure(_, value) => assert(value.json === JString("a"))
    }
  }

  property("string enumerable / positive") {
    val field = "a" :: string("a", "b")

    val json = JObject("a" -> JString("b"))

    val unpickled = field.unpickle(json).get

    assert(unpickled === "b")
    assert(field.pickle("b") === json)
  }

  property("string enumerable / negative") {
    val field = "a" :: string("a", "b")

    val json = JObject("a" -> JString("c"))

    val unpickled = field.unpickle(json)

    unpickled match {
      case s@Success(_, _) => fail(s.toString)
      case Failure(_, value) => assert(value.json === JString("c"))
    }
  }
}
