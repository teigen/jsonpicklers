package jsonpicklers

import org.scalatest.PropSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import net.liftweb.json.JsonAST.{JField, JObject, JInt}

class OrderingTest extends PropSpec with GeneratorDrivenPropertyChecks {
  property("type >") {
    val field = integer > 10

    assert(field.unpickle(JInt(11)) === Success(11, Root, JInt(11)))
    assert(field.pickle(11) === JInt(11))

    assert(field.unpickle(JInt(5)).isFailure === true)
    intercept[RuntimeException] {
      field.pickle(5)
    }
  }

  property("object >") {
    case class Value(i: Int)
    implicit val ordering = Ordering.by[Value, Int](_.i)

    val field = ("name" :: integer).as(Wrap(Value)(Value.unapply(_).get)) > Value(10)

    val json = JObject(List(JField("name", JInt(11))))

    assert(field.unpickle(json) === Success(Value(11), Root, json))
    assert(field.pickle(Value(11)) === json)

    assert(field.unpickle(JObject(List(JField("name", JInt(5))))).isFailure === true)
    intercept[RuntimeException] {
      field.pickle(Value(5))
    }
  }

  property("property >") {
    val field = ("name" :: integer) > 10
    val json = JObject(List(JField("name", JInt(11))))
    assert(field.unpickle(json) === Success(11, Root, json))
    assert(field.pickle(11) === json)

    assert(field.unpickle(JInt(5)).isFailure === true)
    intercept[RuntimeException] {
      field.pickle(5)
    }
  }
}
