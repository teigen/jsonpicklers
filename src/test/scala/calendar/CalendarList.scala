package calendar

import pickles._

/*
 * https://developers.google.com/google-apps/calendar/v3/reference/calendarList
 */
object CalendarList {
  
  import syntax._
  
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

  val wrapCalendarList = wrap(CalendarList.apply)(CalendarList.unapply(_).get)  
  val reminder     = wrap(Reminder)(Reminder.unapply(_).get)

  lazy val calendarList = wrapCalendarList(
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
    ("defaultReminders":: array(reminder(    
      ("method" :: string("email", "sms", "popup")) ~
      ("minutes":: int))    
    ))
  )
}
