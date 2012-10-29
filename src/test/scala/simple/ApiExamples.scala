package simple

import org.scalatest._
import org.scalatest.matchers._
import jsonpicklers.Picklers._
import net.liftweb.json._

class ApiExamples extends FreeSpec with ShouldMatchers {

  "Supports simple values (Hello(\"world\"))" - {

    object Hello {
      val json = wrap(apply)(unapply(_).get){
        "message" :: string
      }
    }
    case class Hello(message:String)

    val json = parse("""{ "message": "world" }""")
    val scala = Hello("world")

    "pickles" in {
      Hello.json.pickle(scala) should equal(json)
    }

    "unpickles" in {
      Hello.json.unpickle(json) match {
        case jsonpicklers.Success(value, _) => value should equal(scala)
        case f => fail(f.toString)
      }
    }
  }

  "Supports optional fields (Hello(Option[\"world\"]))" - {

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

  "Supports maps of named values (Map[String,Obj])" - {
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
