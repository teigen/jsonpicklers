package twitter

import org.scalatest.PropSpec
import io.Source
import org.json4s.native.JsonParser
import jsonpicklers.Result._

class TwitterTest extends PropSpec {
  
  def json(name:String) = {
    val source = Source.fromFile("src/test/resources/twitter/"+name)
    JsonParser.parse(source.bufferedReader())
  }
  
  property("public_timeline.json"){
    val j = json("public_timeline.json") 
    TimeLine.json.unpickle(j) match {
      case Success(value, _) =>
      case f => fail(f.toString)
    }
  }
  
  property("public_timeline?include_entities=true.json"){
    val j = json("public_timeline?include_entities=true.json")
    TimeLine.json.unpickle(j) match {
      case Success(value, _) =>
      case f => fail(f.toString)
    }
  }
}
