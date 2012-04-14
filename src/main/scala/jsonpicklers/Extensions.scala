package jsonpicklers

import net.liftweb.json.JsonAST.JValue
import java.net.URI

trait Extensions {
  val uri = {
    def unpickle(s:String, location:Location, json:JValue) = try{
      Success(new URI(s), location, json)
    } catch {
      case ex => Failure(ex.getMessage, location, json)
    }
    string.flatWrap(unpickle)((p, u) => p(u.toString))
  }
}
