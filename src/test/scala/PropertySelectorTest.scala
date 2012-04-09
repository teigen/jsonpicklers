package jsonpicklers

import org.scalatest.PropSpec
import net.liftweb.json.JsonParser

class PropertySelectorTest extends PropSpec {

  property("* :: string"){
    
    val source = 
    """{
      "fields" : {
        "a" : "Hello",
        "b" : "World"
      }
    }"""    
    
    val json = JsonParser.parse(source)
    
    val fields = "fields" :: 
      (* :: string)
    
    val unpickled = fields.unpickle(json)
    
    val expected = Map("a" -> "Hello", "b" -> "World") 
    
    assert(unpickled === Success(expected, Root, json))
    assert(fields.pickle(expected) === json)
  }
  
  property("* :: (string | integer)"){
    val source =
      """{
        "fields" : {
          "a" : "Hello",
          "b" : 5
        }
      }"""

    val json = JsonParser.parse(source)

    val fields = "fields" :: 
      * :: (string | integer)

    val unpickled = fields.unpickle(json)

    val expected = Map("a" -> "Hello", "b" -> 5)

    assert(unpickled === Success(expected, Root, json))
    assert(fields.pickle(expected) === json)
  }
  
  property("regex"){
    val source = 
    """{
      "fields" : {
        "1" : 1,
        "2" : 2,
        "three" : 3,
        "four" : 4
      }
    }"""
    
    val json = JsonParser.parse(source)
    
    val fields = "fields" :: 
      "\\d".r :: integer
    
    val unpickled = fields.unpickle(json)
    
    val expected = Map("1" -> 1, "2" -> 2)
    
    assert(unpickled === Success(expected, Root, json))
  }
}
