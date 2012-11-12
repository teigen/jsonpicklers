package simple

import org.scalatest._
import org.scalatest.matchers._
import net.liftweb.json._
import jsonpicklers._
import Picklers._

class ApiExamples extends FreeSpec with ShouldMatchers {

  "Supports simple values" - {
    object Hello {
      val json = wrap(apply)(unapply(_).get){
        "message" :: string
      }
    }
    case class Hello(message:String)

    val json = parse("""{ "message": "world" }""")
    val scala = Hello("world")

    "pickles to json" in {
      Hello.json.pickle(scala) should equal(json)
    }

    "unpickles to scala" in {
      Hello.json.unpickle(json) match {
        case jsonpicklers.Success(value, _) => value should equal(scala)
        case f => fail(f.toString)
      }
    }
  }

  "Supports lists of primitives" - {
    object ListOfPrimitives {
      val json = wrap(apply)(unapply(_).get){
        "values" :: int.*
      }
    }
    case class ListOfPrimitives(values:List[Int])

    val json = parse("""{ "values": [1,2,3,4] }""")
    val scala = ListOfPrimitives(List(1,2,3,4))

    "pickles" in {
      ListOfPrimitives.json.pickle(scala) should equal(json)
    }

    "unpickles" in {
      ListOfPrimitives.json.unpickle(json) match {
        case jsonpicklers.Success(value, _) => value should equal(scala)
        case f => fail(f.toString)
      }
    }
  }

  "Supports custom types" - {
    import java.net.URI
    val uri = string.trying(a => Parsers.success(new URI(a)))(u => Some(u.toString))

    object CustomTypes {
      val json = wrap(apply)(unapply(_).get){
        "some_uri" :: uri
      }
    }
    case class CustomTypes(uri:URI)

    val json = parse("""{ "some_uri": "http://example.com/foo?filter=2#bar" }""")
    val scala = CustomTypes(new URI("http://example.com/foo?filter=2#bar"))

    "pickles" in {
      CustomTypes.json.pickle(scala) should equal(json)
    }

    "unpickles" in {
      CustomTypes.json.unpickle(json) match {
        case jsonpicklers.Success(value, _) => value should equal(scala)
        case f => fail(f.toString)
      }
    }

    "pickle negative" in {
      val unpickled = CustomTypes.json.unpickle(JObject(List(JField("some_uri", JString("\\")))))
      unpickled.fold(
        (msg, _) => assert(msg === """Illegal character in path at index 0: \"""),
        (ok, _)  => fail("expected failure, got " + ok))
    }
  }

  "Supports optionally missing fields" - {

    object Hello {
      val json = wrap(apply)(unapply(_).get){
        ("message" :: string).?
      }
    }
    case class Hello(message:Option[String])

    val json = parse("""{ "message": "world" }""")
    val json2 = parse("""{ }""")
    val scala = Hello(Some("world"))
    val scala2 = Hello(None)

    "pickles" in {
      Hello.json.pickle(scala) should equal(json)
      Hello.json.pickle(scala2) should equal(json2)
    }

    "unpickles" in {
      Hello.json.unpickle(json) match {
        case jsonpicklers.Success(value, _) => value should equal(scala)
        case f => fail(f.toString)
      }
      Hello.json.unpickle(json2) match {
        case jsonpicklers.Success(value, _) => value should equal(scala2)
        case f => fail(f.toString)
      }
    }
  }

  "Supports maps of named values (Map[String,A])" - {
    object Size {
      val json = wrap(apply)(unapply(_).get){
        ("x" :: int) ~
          ("y" :: int)
      }
    }
    case class Size(x:Int, y:Int)

    object Sizes {
      val json = wrap(apply)(unapply(_).get){
        ("sizes" :: ( * :: Size.json))
      }
    }
    case class Sizes(sizes:Map[String,Size])

    val json = parse("""
                    {
                      "sizes" : {
                        "small": { "x": 200, "y": 200 },
                        "medium":{ "x": 400, "y": 400 },
                        "large": { "x": 600, "y": 600 }
                      }
                    }
                    """)

    val scala = Sizes(Map("small" -> Size(200, 200), "medium" -> Size(400, 400), "large" -> Size(600, 600)))

    "pickles" in {
      Sizes.json.pickle(scala) should equal(json)
    }

    "unpickles" in {
      Sizes.json.unpickle(json) match {
        case jsonpicklers.Success(value, _) => value should equal(scala)
        case f => fail(f.toString)
      }
    }
  }
}
