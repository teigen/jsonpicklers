package jsonpicklers

import org.scalatest.PropSpec
import net.liftweb.json.JsonParser
import net.liftweb.json.JsonAST._

class OrTest extends PropSpec {

  import tuples._

  property("or") {
    sealed trait Value
    case class StringValue(value: String) extends Value
    case class IntValue(value: Int) extends Value
    case class TwoValues(a: Value, b: Value)

    implicit val wrapStringValue = Wrap(StringValue)(StringValue.unapply(_).get)
    implicit val wrapIntValue = Wrap(IntValue)(IntValue.unapply(_).get)
    implicit val wrapTwoValues = Wrap(TwoValues)(TwoValues.unapply(_).get)

    implicit val reifyIntValue = Reify {
      case i: IntValue => i
    }
    implicit val reifyStringValue = Reify {
      case s: StringValue => s
    }

    val stringValue = string.as[StringValue]
    val intValue = integer.as[IntValue]
    val value: JsonType[Value] = stringValue | intValue
    val a = "a" :: value
    val b = "b" :: value

    val test = (a ~ b).as[TwoValues](wrapTwoValues)

    val input = JsonParser.parse("""{"a":1, "b":"Hello"}""")
    val unpickled: Result[TwoValues] = test.unpickle(input)

    unpickled match {
      case Success(s, _, _) =>
        assert(s === TwoValues(IntValue(1), StringValue("Hello")))
        assert(test.pickle(s) === input)
      case f => fail(f.toString)
    }
  }

  property("default values and or-else") {
    val field = ("bool" :: boolean).?(false)
    assert(field.unpickle(JObject(Nil)) === Success(false, Root, JObject(Nil)))
    assert(field.pickle(true) === JObject(List(JField("bool", JBool(true)))))
    assert(field.pickle(false) === JObject(List(JField("bool", JBool(false)))))
  }
}
