package simple

import org.scalatest._
import org.scalatest.matchers._
import net.liftweb.json.JsonParser

class APISpec extends FreeSpec with ShouldMatchers {

  "Hello(\"world\")" - {
    val json = j("""{ "message": "world" }""")
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

  "Mapped subtypes" - {
    val json = j(
      """
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

  def j(s:String) = {
    JsonParser.parse(s)
  }

}
