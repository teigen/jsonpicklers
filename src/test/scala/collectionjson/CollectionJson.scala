package collectionjson

import java.net.{URISyntaxException, URI}
import net.liftweb.json.JsonAST.{JValue}
import jsonpicklers._

object CollectionJson {

  import tuples._

  case class CollectionJson(version: String, href: URI, links: List[Link], items: List[Item], queries: List[Query], template: Option[Template], error: Option[Error])

  case class Error(title: Option[String], code: Option[String], message: Option[String])

  case class Template(data: List[Data])

  case class Item(href: URI, data: List[Data], links: List[Link])

  case class Data(name: Option[String], value: Option[String], prompt: Option[String])

  case class Query(href: URI, rel: String, name: Option[String], prompt: Option[String], data: List[Data])

  case class Link(href: URI, rel: String, name: Option[String], render: Option[String], prompt: Option[String])

  implicit val wrapCollectionJson = Wrap(CollectionJson)(CollectionJson.unapply(_).get)
  implicit val wrapError = Wrap(Error)(Error.unapply(_).get)
  implicit val wrapTemplate = Wrap(Template)(Template.unapply(_).get)
  implicit val wrapItem = Wrap(Item)(Item.unapply(_).get)
  implicit val wrapData = Wrap(Data)(Data.unapply(_).get)
  implicit val wrapQuery = Wrap(Query)(Query.unapply(_).get)
  implicit val wrapLink = Wrap(Link)(Link.unapply(_).get)

  lazy val collection = "collection" :: (version ~ href ~ links.?(Nil) ~ items.?(Nil) ~ queries.?(Nil) ~ template.? ~ error.?).as[CollectionJson]

  lazy val error = "error" :: (title.? ~ code.? ~ message.?).as[Error]

  lazy val template = "template" :: data.as[Template]

  lazy val items = "items" :: array((href ~ data.?(Nil) ~ links.?(Nil)).as[Item])

  lazy val data = "data" :: array((name.? ~ value.? ~ prompt.?).as[Data])

  lazy val queries = "queries" :: array((href ~ rel ~ name.? ~ prompt.? ~ data.?(Nil)).as[Query])

  lazy val links = "links" :: array((href ~ rel ~ name.? ~ render.? ~ prompt.?).as[Link])

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

  val URI = {
    def unpickle(s: String, location: Location, json: JValue) = try {
      Success(new URI(s), location, json)
    } catch {
      case ex: URISyntaxException => Failure(ex.getMessage, location, json)
    }
    string.flatWrap(unpickle)(_.toString)
  }
}
