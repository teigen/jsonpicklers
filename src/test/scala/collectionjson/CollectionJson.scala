package collectionjson

import java.net.{URISyntaxException, URI}
import pickles._
import syntax._

object CollectionJson {

  case class CollectionJson(version: String, href: URI, links: List[Link], items: List[Item], queries: List[Query], template: Option[Template], error: Option[Error])

  case class Error(title: Option[String], code: Option[String], message: Option[String])

  case class Template(data: List[Data])

  case class Item(href: URI, data: List[Data], links: List[Link])

  case class Data(name: Option[String], value: Option[String], prompt: Option[String])

  case class Query(href: URI, rel: String, name: Option[String], prompt: Option[String], data: List[Data])

  case class Link(href: URI, rel: String, name: Option[String], render: Option[String], prompt: Option[String])

  lazy val collection = "collection" :: wrapper(version ~ href ~ links.?(Nil) ~ items.?(Nil) ~ queries.?(Nil) ~ template.? ~ error.?)(CollectionJson)(CollectionJson.unapply(_).get)

  lazy val error = "error" :: wrapper(title.? ~ code.? ~ message.?)(Error)(Error.unapply(_).get)

  lazy val template = "template" :: wrapper(data)(Template)(Template.unapply(_).get)

  lazy val items = "items" :: array(wrapper(href ~ data.?(Nil) ~ links.?(Nil))(Item)(Item.unapply(_).get))

  lazy val data = "data" :: array(wrapper(name.? ~ value.? ~ prompt.?)(Data)(Data.unapply(_).get))

  lazy val queries = "queries" :: array(wrapper(href ~ rel ~ name.? ~ prompt.? ~ data.?(Nil))(Query)(Query.unapply(_).get))

  lazy val links = "links" :: array(wrapper(href ~ rel ~ name.? ~ render.? ~ prompt.?)(Link)(Link.unapply(_).get))

  lazy val code = "code" :: string

  lazy val href = "href" :: URI

  lazy val message = "message" :: string

  lazy val name = "name" :: string

  lazy val prompt = "prompt" :: string

  lazy val rel = "rel" :: string

  lazy val render = "render" :: string

  lazy val title = "title" :: string

  lazy val value = "value" :: string

  lazy val version = "version" :: string

  val URI = new JsonValue[URI]{
    def pickle(a: URI) = string.pickle(a.toString)
    def unpickle(location: Location) = string.unpickle(location).flatMap{ s =>
      try{
        Success(new URI(s), location)
      } catch {
        case ex:URISyntaxException => Failure(ex.getMessage, location)
      }
    }
  }
}
