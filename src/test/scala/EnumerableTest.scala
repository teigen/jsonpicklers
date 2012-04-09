package jsonpicklers

import org.scalatest.PropSpec
import net.liftweb.json.JsonAST._
import syntax._

class EnumerableTest extends PropSpec {
  property("string literal / positive"){
    
    val field = "a" :: "b"
    
    val json = JObject(List(JField("a", JString("b"))))
    
    val unpickled = field.unpickle(json)
    
    assert(unpickled === Success("b", Root, json))
    
    assert(field.pickle("b") == json)    
  }
  
  property("string literal / negative"){
    val field = "a" :: "b"
    
    val json = JObject(List(JField("a", JString("a"))))
    
    val unpickled = field.unpickle(json)
    
    unpickled match {
      case s@Success(_, _, _) => fail(s.toString)
      case Failure(_, _, value) => assert(value === JString("a"))
    }
  }
  
  property("string enumerable / positive"){
    val field = "a" :: string("a", "b")
    
    val json = JObject(List(JField("a", JString("b"))))
    
    val unpickled = field.unpickle(json)
    
    assert(unpickled === Success("b", Root, json))
    assert(field.pickle("b") === json)
  }
  
  property("string enumerable / negative"){
    val field = "a" :: string("a", "b")
    
    val json = JObject(List(JField("a", JString("c"))))
    
    val unpickled = field.unpickle(json)
    
    unpickled match {
      case s@Success(_, _, _) => fail(s.toString)
      case Failure(_, _, value) => assert(value === JString("c"))
    }
  }
}
