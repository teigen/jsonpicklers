package jsonpicklers

import Picklers._

import org.scalatest.PropSpec
import org.json4s.JsonAST._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.Gen.oneOf
import org.scalacheck.Arbitrary.arbitrary

class OrTest extends PropSpec with GeneratorDrivenPropertyChecks {
  
  property("or - multiple"){
    forAll(oneOf(arbitrary[Int], arbitrary[String], arbitrary[Double])){ value:Any =>
      val element = int | string | double
      val pickled = element.pickle(value)
      assert(element.unpickle(pickled).get === value)
    }
  }

  property("or - wrapped") {
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

    val input = JObject("a" -> JInt(1), "b" -> JString("Hello"))
    val unpickled: Result[TwoValues] = test.unpickle(input)
    
    val expected = TwoValues(IntValue(1), StringValue("Hello"))

    assert(unpickled.get === expected)
    assert(test.pickle(expected) === input)
  }

  property("default values and or-else") {
    val field = ("bool" :: boolean).?(false)
    assert(field.unpickle(JObject(Nil)).get === false)
    assert(field.pickle(true) === JObject("bool" -> JBool(true)))
    assert(field.pickle(false) === JObject("bool" -> JBool(false)))
  }

  property("ignored default values on pickle (field ?? <i>)") {
    forAll{ (a:Int, b:Int) =>
      val field = ("num" :: int) ?? a
      val pickled = field.pickle(b)
      if (a == b)
        assert(pickled === JObject())
      else
        assert(pickled === JObject("num" -> JInt(b)))
    }
  }
}
