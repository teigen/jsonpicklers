package jsonpicklers

import Picklers._

import org.scalatest.PropSpec
import net.liftweb.json.JsonParser
import net.liftweb.json.JsonAST._
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class OrTest extends PropSpec with GeneratorDrivenPropertyChecks {
  
  property("or - multiple"){
    forAll{ (i:Int, s:String, d:Double, r:Int) =>
      val element = int | string | double
      val value = math.abs(r % 3) match {
        case 0 => i
        case 1 => s
        case 2 => d 
      }
      val pickled = element.pickle(value)
      assert(element.unpickle(pickled).get === value)
    }
  }

  property("or") {
    sealed trait Value
    case class StringValue(value: String) extends Value
    case class IntValue(value: Int) extends Value
    case class TwoValues(a: Value, b: Value)

    val wrapStringValue = wrap(StringValue)(StringValue.unapply(_).get)
    val wrapIntValue    = wrap(IntValue)(IntValue.unapply(_).get)
    val wrapTwoValues   = wrap(TwoValues)(TwoValues.unapply(_).get)

    val stringValue = string ^^ wrapStringValue
    val intValue    = int    ^^ wrapIntValue
    val value: JsonValue[Value] = stringValue | intValue
    val a = "a" :: value
    val b = "b" :: value

    val test = a ~ b ^^ wrapTwoValues

    val input = JsonParser.parse("""{"a":1, "b":"Hello"}""")
    val unpickled: Result[TwoValues] = test.unpickle(input)
    
    val expected = TwoValues(IntValue(1), StringValue("Hello"))

    assert(unpickled.get === expected)
    assert(test.pickle(expected) === input)
  }

  property("default values and or-else") {
    val field = ("bool" :: boolean).?(false)
    assert(field.unpickle(JObject(Nil)).get === false)
    assert(field.pickle(true) === JObject(List(JField("bool", JBool(true)))))
    assert(field.pickle(false) === JObject(List(JField("bool", JBool(false)))))
  }

  property("field ?? <i>") {
    forAll{ (a:Int, b:Int) =>
      val field = ("num" :: int) ?? a
      val pickled = field.pickle(b)
      if (a == b)
        assert(pickled === JObject(Nil))
      else
        assert(pickled === JObject(List(JField("num", JInt(b)))) )
    }
  }
}
