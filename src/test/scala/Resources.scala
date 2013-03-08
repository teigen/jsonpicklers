package jsonpicklers

import io.Source
import org.json4s.native.JsonParser
import org.json4s.JsonAST.JValue

trait Resources {
  def source(name:String):JValue =
    JsonParser.parse(Source.fromFile("src/test/resources/" + name).reader())

}
