package calendar

import pickles._

/*
 * https://developers.google.com/google-apps/calendar/v3/reference/calendarList
 */
object CalendarList {
  
  val etag = string
  
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
  
  case class Reminder(method:String, minutes:Int)

  lazy val calendarList = (
    ("kind"            :: string("calendar#calendarListEntry")) ~
    ("etag"            :: etag)   ~
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
      ("minutes":: int)).wrap(Reminder)(Reminder.unapply(_).get)    
    ))
  ).wrap(CalendarList.apply)(CalendarList.unapply(_).get)
}
