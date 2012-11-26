package examples

import org.json4s.native.JsonParser.parse

import jsonpicklers._
import Picklers._
import org.scalatest.FunSuite

class PersonExamples extends FunSuite {
  val json =
    """
      |{ "name": "joe",
      |  "address": {
      |    "street": "Bulevard",
      |    "city": "Helsinki"
      |  },
      |  "children": [
      |    {
      |      "name": "Mary",
      |      "age": 5
      |      "birthdate": "2004-09-04T18:06:22Z"
      |    },
      |    {
      |      "name": "Mazy",
      |      "age": 3
      |    }
      |  ]
      |}
    """.stripMargin

  private def format = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'")

  val date = string.trying(a => Parsers.success(format.parse(a)))(d => Some(format.format(d)))

  object Child {
    val json = wrap(apply)(unapply(_).get){
      ("name"      :: string) ~
      ("age"       :: int) ~
      ("birthdate" :: date).?
    }
  }
  case class Child(name: String, age: Int, birthdate: Option[java.util.Date])

  object Address {
    val json = wrap(apply)(unapply(_).get){
      ("street" :: string) ~
      ("city"   :: string)
    }
  }
  case class Address(street: String, city: String)

  object Person {
    val json = wrap(apply)(unapply(_).get){
      ("name"     :: string) ~
      ("address"  :: Address.json) ~
      ("children" :: Child.json.*)
    }
  }
  case class Person(name: String, address: Address, children: List[Child])

  test("pickle/unpickle"){
    val parsed        = parse(json)
    val Success(p, _) = Person.json.unpickle(parsed)
    val pickled       = Person.json.pickle(p)
    assert(parsed === pickled)
  }
}
