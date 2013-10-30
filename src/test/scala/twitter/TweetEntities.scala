package twitter

import jsonpicklers._
import Picklers._

import java.net.{MalformedURLException, URL}

object Types {
  val url = {
    def noProtocol(s:String) = "no protocol:.*".r.pattern.matcher(s).matches()
    def tryPickle(a: URL) = string.tryPickle(a.toString)
    def unpickle = Parser{ location =>
      def un(s:String, location:Location):Result[URL] = try{
        Success(new URL(s), location)
      } catch {
        case ex:MalformedURLException if noProtocol(ex.getMessage) => un("http://" + s, location)
        case ex:Exception => Failure(ex.getMessage, location)
      }      
      string.unpickle(location).flatMap{ s => un(s, location) }
    }
    JsonValue(unpickle, tryPickle)
  }  
}

import Types._

object Entities {
  val json = wrap(apply)(unapply(_).get){
    ("urls"          :: TweetURL.json.*) ~
    ("user_mentions" :: array(UserMention.json)) ~
    ("hashtags"      :: array(HashTag.json)) ~
    ("media"         :: array(Media.json)).?(Nil)
  } 
}

case class Entities(urls:List[TweetURL], 
                    userMentions:List[UserMention], 
                    hashtags:List[HashTag], 
                    media:List[Media])

object Media {
  val json = wrap(apply)(unapply(_).get){
    ("id"              :: int) ~
    ("id_str"          :: string) ~
    ("media_url"       :: url) ~
    ("media_url_https" :: url) ~
    ("url"             :: url) ~
    ("dislplay_url"    :: url) ~
    ("expanded_url"    :: url) ~
    ("sizes"           :: (* :: Size.json)) ~
    ("type"            :: string("photo")) ~
    ("indices"         :: array(int))
  }
}

case class Media(id:Int, idStr:String, 
                 mediaURL:URL, 
                 mediaURLHttps:URL,
                 url:URL,
                 displayURL:URL,
                 expandedURL:URL,
                 sizes:Map[String, Size],
                 mediaType:String,
                 indices:List[Int])

object Size {
  val json = wrap(apply)(unapply(_).get){
    ("w"      :: int) ~
    ("h"      :: int) ~
    ("resize" :: string("crop", "fit"))
  }
}

case class Size(w:Int, 
                h:Int, 
                resize:String)

object TweetURL {
  val json = wrap(apply)(unapply(_).get){
    ("url"          :: url) ~
    ("display_url"  :: url) ~
    ("expanded_url" :: url) ~
    ("indices"      :: array(int))
  }
}

case class TweetURL(url:URL, 
                    displayURL:URL, 
                    expandedURL:URL, 
                    indices:List[Int])

object UserMention {
  val json = wrap(apply)(unapply(_).get){
    ("id"          :: int) ~
    ("id_str"      :: string) ~
    ("screen_name" :: string) ~
    ("name"        :: string) ~ 
    ("indices"     :: array(int))
  }
}

case class UserMention(id:Int, 
                       idStr:String, 
                       screenName:String, 
                       name:String, 
                       indices:List[Int])


object HashTag {
  val json = wrap(apply)(unapply(_).get){  
    ("text"    :: string) ~
    ("indices" :: array(int))
  }  
}

case class HashTag(text:String, 
                   indices:List[Int])

/* timeline */
object TimeLine {
  val json = wrap(apply)(unapply(_).get){
    array(Tweet.json)
  }
}
case class TimeLine(tweets:List[Tweet])

object Tweet {
  val json = wrap(apply)(unapply(_).get){
    ("id"                        :: bigint) ~
    ("id_str"                    :: string) ~
    ("created_at"                :: string) ~
    ("favorited"                 :: boolean) ~
    ("in_reply_to_screen_name"   :: (string | NULL)) ~
    ("in_reply_to_user_id"       :: (bigint | NULL)) ~
    ("in_reply_to_user_id_str"   :: (string | NULL)) ~
    ("in_reply_to_status_id"     :: (bigint | NULL)) ~
    ("in_reply_to_status_id_str" :: (string | NULL)) ~
    ("text"                      :: string) ~
    ("retweet_count"             :: int) ~
    ("truncated"                 :: boolean) ~
    ("retweeted"                 :: boolean) ~
    ("source"                    :: string) ~
    ("entities"                  :: Entities.json).? ~
    ("user"                      :: User.json)
  }
  //        ("place"                     :: NULL)
  //        ("geo"                       :: NULL)
  //        ("coordinates"               :: NULL)
  //        ("contributors"              :: NULL)
}

case class Tweet(id: BigInt,
                 idStr: String,
                 createAt: String,
                 favorited: Boolean,
                 inReplyToScreenName: String,
                 inReplyToUserId: BigInt,
                 inReplyToUserIdStr: String,
                 inReplyToStatusId: BigInt,
                 inReplyToStatusIdStr: String,
                 text: String, retweetCount: Int,
                 truncated: Boolean,
                 retweeted: Boolean,
                 source: String,
                 entities:Option[Entities],
                 user: User)

object User {
  val json = wrap(apply)(unapply(_).get){
    // Need to chop chop! (22)
    UserData.json ~ UserCount.json ~ UserProfile.json ~ UserFlags.json
  } 
}

// Need to chop chop! (22)
case class User(data:UserData, count:UserCount, profile:UserProfile, flags:UserFlags)


object UserData {
    val json = wrap(apply)(unapply(_).get){
      ("id"          :: bigint ) ~
      ("id_str"      :: string ) ~
      ("url"         :: (url | NULL)) ~
      ("created_at"  :: string ) ~
      ("utc_offset"  :: option(int)) ~
      ("screen_name" :: string ) ~
      ("name"        :: string ) ~
      ("time_zone"   :: option(string)) ~
      ("location"    :: option(string) ) ~
      ("lang"        :: string ) ~
      ("description" :: option(string) )
      
    }
}

case class UserData(id:BigInt, 
                    idStr:String, 
                    url:URL, 
                    createdAt:String, 
                    utcOffset:Option[Int], 
                    screenName:String, 
                    name:String, 
                    timeZone:Option[String], 
                    location:Option[String], 
                    lang:String, 
                    description:Option[String])
      
object UserCount {
  val json = wrap(apply)(unapply(_).get){
    ("listed_count"     :: int) ~
    ("friends_count"    :: int) ~
    ("statuses_count"   :: int) ~
    ("followers_count"  :: int) ~
    ("favourites_count" :: int)
  }
}

case class UserCount(listed:Int, friends:Int, statuses:Int, followers:Int, favourites:Int)

object UserProfile {
  val json = wrap(apply)(unapply(_).get){
    ("profile_use_background_image"       :: boolean) ~
    ("profile_background_image_url_https" :: url    ) ~
    ("profile_text_color"                 :: string ) ~
    ("profile_sidebar_border_color"       :: string ) ~
    ("profile_image_url"                  :: url    ) ~
    ("profile_background_tile"            :: boolean) ~
    ("default_profile_image"              :: boolean) ~
    ("profile_sidebar_fill_color"         :: string ) ~
    ("profile_background_color"           :: string ) ~
    ("profile_image_url_https"            :: url    ) ~
    ("profile_background_image_url"       :: url    ) ~
    ("default_profile"                    :: boolean) ~
    ("profile_link_color"                 :: string )
  }
}

case class UserProfile(useBackgroundImage: Boolean,
                       backgroundImageUrlHttps: URL,
                       textColor: String,
                       sidebarCorderColor: String,
                       imageURL: URL,
                       backgroundTile: Boolean,
                       profileImage: Boolean,
                       sidebarFillColor: String,
                       backgroundColor: String,
                       imageUrlHttps: URL,
                       backgroundImageURL: URL,
                       default: Boolean,
                       linkColor: String)

object UserFlags {
  val json = wrap(apply)(unapply(_).get)(
    ("protected"             :: boolean) ~
    ("geo_enabled"           :: boolean) ~
    ("contributors_enabled"  :: boolean) ~
    ("is_translator"         :: boolean) ~
    ("show_all_inline_media" :: option(boolean)) ~
    ("notifications"         :: (boolean ? false)) ~
    ("follow_request_sent"   :: (boolean ? false)) ~
    ("following"             :: (boolean ? false)) ~
    ("verified"              :: boolean)
  )
}
case class UserFlags(`protected`:Boolean, 
                     geoEnabled:Boolean, 
                     contributorsEnabled:Boolean,
                     isTranslator:Boolean,
                     showAllInlineMedia:Option[Boolean],
                     notifications:Boolean,
                     followRequestSent:Boolean, 
                     following:Boolean, 
                     verified:Boolean)

