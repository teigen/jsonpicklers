package calendar

import jsonpicklers._
import Picklers._

/*
 * https://developers.google.com/google-apps/calendar/v3/reference/calendarList
 */
object CalendarList {
  
  object CalendarList{
    val json = xmap(apply)(unapply(_).get)
  }
  case class CalendarList(kind:String,
                          etag:String, 
                          id:String, 
                          summary:String, 
                          description:Option[String], 
                          location:Option[String], 
                          timeZone:Option[String], 
                          summaryOverride:Option[String], 
                          colorId:Option[String], 
                          hidden:Boolean, 
                          selected:Boolean, 
                          accessRole:String, 
                          defaultReminders:List[Reminder])
  
  object Reminder {
    val json = xmap(apply)(unapply(_).get)
  }
  case class Reminder(method:String, minutes:Int)

  lazy val calendarList = (
    ("kind"            :: string("calendar#calendarListEntry")) ~
    ("etag"            :: string) ~
    ("id"              :: string) ~
    ("summary"         :: string) ~
    ("description"     :: string).? ~
    ("location"        :: string).? ~
    ("timeZone"        :: string).? ~
    ("summaryOverride" :: string).? ~
    ("colorId"         :: string).? ~
    ("hidden"          :: boolean).?(false) ~
    ("selected"        :: boolean).?(false) ~
    ("accessRole"      :: string("freeBusyReader", "reader", "writer", "owner")) ~
    ("defaultReminders":: array((    
      ("method" :: string("email", "sms", "popup")) ~
      ("minutes":: int)) ^^ Reminder.json
    ))
  ) ^^ CalendarList.json
}
