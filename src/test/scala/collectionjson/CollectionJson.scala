package collectionjson

import java.net.{URISyntaxException, URI}
import pickles._
import Picklers._

object CollectionJson {

  case class CollectionJson(version: String, href: URI, links: List[Link], items: List[Item], queries: List[Query], template: Option[Template], error: Option[Error])

  case class Error(title: Option[String], code: Option[String], message: Option[String])

  case class Template(data: List[Data])

  case class Item(href: URI, data: List[Data], links: List[Link])

  case class Data(name: Option[String], value: Option[String], prompt: Option[String])

  case class Query(href: URI, rel: String, name: Option[String], prompt: Option[String], data: List[Data])

  case class Link(href: URI, rel: String, name: Option[String], render: Option[String], prompt: Option[String])

  lazy val collection = "collection" :: {
    version        ~ 
    href           ~ 
    links.?(Nil)   ~ 
    items.?(Nil)   ~ 
    queries.?(Nil) ~ 
    template.?     ~ 
    error.?
  }.wrap(CollectionJson)(CollectionJson.unapply(_).get)

  lazy val error = "error" :: {
    title.? ~ 
    code.?  ~ 
    message.?
  }.wrap(Error)(Error.unapply(_).get)

  lazy val template = "template" :: data.wrap(Template)(Template.unapply(_).get)

  lazy val items = "items" :: array({
    href        ~ 
    data.?(Nil) ~ 
    links.?(Nil)
  }.wrap(Item)(Item.unapply(_).get))

  lazy val data = "data" :: array({
    name.?  ~ 
    value.? ~ 
    prompt.?
  }.wrap(Data)(Data.unapply(_).get))

  lazy val queries = "queries" :: array({
    href     ~ 
    rel      ~ 
    name.?   ~ 
    prompt.? ~ 
    data.?(Nil)
  }.wrap(Query)(Query.unapply(_).get))

  lazy val links = "links" :: array({
    href     ~ 
    rel      ~ 
    name.?   ~ 
    render.? ~ 
    prompt.?
  }.wrap(Link)(Link.unapply(_).get))

  lazy val code    = "code"    :: string
  lazy val href    = "href"    :: uri
  lazy val message = "message" :: string
  lazy val name    = "name"    :: string
  lazy val prompt  = "prompt"  :: string
  lazy val rel     = "rel"     :: string
  lazy val render  = "render"  :: string
  lazy val title   = "title"   :: string
  lazy val value   = "value"   :: string
  lazy val version = "version" :: string

  val uri = {
    def tryPickle(a: URI) = string.tryPickle(a.toString)
    def unpickle = Parser{ location => 
      string.unpickle(location).flatMap{ s =>
        try{
          Success(new URI(s), location)
        } catch {
          case ex:URISyntaxException => Failure(ex.getMessage, location)
        }
      }
    }
    JsonValue(unpickle, tryPickle)
  }
}
