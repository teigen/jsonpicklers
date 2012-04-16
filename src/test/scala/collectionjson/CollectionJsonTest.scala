package collectionjson

import org.scalatest.PropSpec
import io.Source
import net.liftweb.json.JsonParser
import pickles._

class CollectionJsonTest extends PropSpec {

  def source(name: String) = JsonParser.parse(Source.fromFile("src/test/resources/" + name).bufferedReader())

  def json(name: String) = {
    collectionjson.CollectionJson.collection.unpickle(source(name)) match {
      case Success(_, _) =>
      //      println(s)

      case f: Failure => fail(f.toString)
    }
  }

  property("minimal") {
    json("collectionjson/1-minimal.json")
  }

  property("collection") {
    json("collectionjson/2-collection.json")
  }

  property("item") {
    json("collectionjson/3-item.json")
  }

  property("queries") {
    json("collectionjson/4-queries.json")
  }

  property("template") {
    json("collectionjson/5-template.json")
  }

  property("error") {
    json("collectionjson/6-error.json")
  }
}