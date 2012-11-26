package jsonpicklers

import Picklers._

import org.scalatest.PropSpec
import org.json4s.native.JsonParser

class PropertySelectorTest extends PropSpec {

  property("* :: string") {

    val source =
      """{
        "fields" : {
          "a" : "Hello",
          "b" : "World"
        }
      }"""

    val json = JsonParser.parse(source)

    val fields = "fields" ::
      (* :: string)

    val unpickled = fields.unpickle(json).get

    val expected = Map("a" -> "Hello", "b" -> "World")

    assert(unpickled === expected)
    assert(fields.pickle(expected) === json)
  }

  property("* :: (string | integer)") {
    val source =
      """{
        "fields" : {
          "a" : "Hello",
          "b" : 5
        }
      }"""

    val json = JsonParser.parse(source)

    val fields = "fields" ::
      * :: (string | int)

    val unpickled = fields.unpickle(json).get

    val expected = Map("a" -> "Hello", "b" -> 5)

    assert(unpickled === expected)
    assert(fields.pickle(expected) === json)
  }

  property("regex") {
    val source =
      """{
        "fields" : {
          "1" : 1,
          "2" : 2,
          "three" : 3,
          "four" : 4
        }
      }"""

    val json = JsonParser.parse(source)

    val fields = "fields" ::
      "\\d".r :: int

    val unpickled = fields.unpickle(json).get

    val expected = Map("1" -> 1, "2" -> 2)

    assert(unpickled === expected)
  }
}
