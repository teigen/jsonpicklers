package simple

import org.scalatest._
import org.scalatest.matchers._
import net.liftweb.json.JsonParser

class APISpec extends FreeSpec with ShouldMatchers {

  "Hello(\"world\")" - {
    val json = j("""{ "message": "world" }""")

    "pickles" in {
      Hello.json.pickle(Hello("world")) should equal(json)
    }

    "unpickles" in {
      Hello.json.unpickle(json) match {
        case jsonpicklers.Success(value, _) => value should equal(Hello("world"))
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

    val struct = Sizes(Map("small" -> Size(200, 200), "medium" -> Size(400, 400), "large" -> Size(600, 600)))

    "pickles" in {
      Sizes.json.pickle(struct) should equal(json)
    }

    "unpickles" in {
      Sizes.json.unpickle(json) match {
        case jsonpicklers.Success(value, _) => value should equal(struct)
        case f => fail(f.toString)
      }
    }
  }

  def j(s:String) = {
    JsonParser.parse(s)
  }

}
