package jsonpicklers

import Picklers._

import org.json4s.JsonAST.{JObject, JInt}
import org.scalacheck._
import org.scalacheck.Prop._

object OrderingTest extends Properties("ordering") {
  
  property("value >") = forAll{ (a:Int, b:Int) =>
    val field = int > b
    (a > b) ==> (field.pickle(a) == JInt(a))      :| "pickle" &&
    (a > b) ==> field.unpickle(JInt(a)).isSuccess :| "unpickle"
  }
       
  property("value > (failure)") = forAll{ (a:Int, b:Int) =>
    val field = int > b
    !(a > b) ==> (field.pickle(a) throws classOf[RuntimeException]) :| "pickle" &&
    !(a > b) ==> field.unpickle(JInt(a)).isFailure                  :| "unpickle"
  }

  case class Value(value:Int)
  implicit val ordering = Ordering.by[Value, Int](_.value)
  val value = ("value" :: int).wrap(Value)(Value.unapply(_).get)  
  
  property("object >") = forAll{ (a:Int, b:Int) =>
    val obj = value > Value(b)
    val json = JObject("value" -> JInt(a))
    
    (a > b) ==> (obj.pickle(Value(a)) == json) :| "pickle" &&
    (a > b) ==> obj.unpickle(json).isSuccess   :| "unpickle"
  }
  
  property("object > (failure)") = forAll{ (a:Int, b:Int) =>
    val obj = value > Value(b)
    val json = JObject("value" -> JInt(a))
    
    !(a > b) ==> (obj.pickle(Value(a)) throws classOf[RuntimeException]) :| "pickle" &&
    !(a > b) ==> obj.unpickle(json).isFailure                            :| "unpickle"
  }


//
//  property("property >") {
//    val field = ("name" :: int) > 10
//    val json = JObject(List(JField("name", JInt(11))))
//    assert(field.unpickle(json) === Success(11, Root(json)))
//    assert(field.pickle(11) === json)
//
//    assert(field.unpickle(JInt(5)).isFailure === true)
//    intercept[RuntimeException] {
//      field.pickle(5)
//    }
//  }
}
