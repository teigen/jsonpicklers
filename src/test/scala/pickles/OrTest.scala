package pickles

import org.scalatest.PropSpec
import net.liftweb.json.JsonParser
import net.liftweb.json.JsonAST._

class OrTest extends PropSpec {

  property("or") {
    sealed trait Value
    case class StringValue(value: String) extends Value
    case class IntValue(value: Int) extends Value
    case class TwoValues(a: Value, b: Value)

    val wrapStringValue = wrap(StringValue)(StringValue.unapply(_).get)
    val wrapIntValue = wrap(IntValue)(IntValue.unapply(_).get)
    val wrapTwoValues = wrap(TwoValues)(TwoValues.unapply(_).get)

    implicit val reifyIntValue = Reify {
      case i: IntValue => i
    }
    implicit val reifyStringValue = Reify {
      case s: StringValue => s
    }

    val stringValue = wrapStringValue(string)
    val intValue = wrapIntValue(int)
    val value: JsonValue[Value] = stringValue | intValue
    val a = "a" :: value
    val b = "b" :: value

    val test = wrapTwoValues(a ~ b)

    val input = JsonParser.parse("""{"a":1, "b":"Hello"}""")
    val unpickled: Result[TwoValues] = test.unpickle(input)

    unpickled match {
      case Success(s, _) =>
        assert(s === TwoValues(IntValue(1), StringValue("Hello")))
        assert(test.pickle(s) === input)
      case f => fail(f.toString)
    }
  }

  property("default values and or-else") {
    val field = ("bool" :: boolean).?(false)
    assert(field.unpickle(JObject(Nil)) === Success(false, Root(JObject(Nil))))
    assert(field.pickle(true) === JObject(List(JField("bool", JBool(true)))))
    assert(field.pickle(false) === JObject(List(JField("bool", JBool(false)))))
  }
}
