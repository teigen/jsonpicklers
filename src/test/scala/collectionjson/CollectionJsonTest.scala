package collectionjson

import org.scalatest.PropSpec
import jsonpicklers._

class CollectionJsonTest extends PropSpec with Resources {

  def json(name: String) = {
    collectionjson.CollectionJson.collection.unpickle(source(name)) match {
      case Result.Success(_, _) =>
      //      println(s)

      case f: Result.Failure => fail(f.toString)
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