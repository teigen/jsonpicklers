package calendar

import jsonpicklers._
import Picklers._

/*
 * https://developers.google.com/google-apps/calendar/v3/reference/events
 */
object Events {
  val etag     = string
  val datetime = string
  val date     = string

  case class Gadget(tpe:String,
                    title:String,
                    link:String,
                    iconLink:String,
                    width:Option[Int],
                    height:Option[Int],
                    display:Option[String],
                    preferences:Map[String, String])
  
  case class ExtendedProperties(prvate:Map[String, String], 
                                shared:Map[String, String])

  case class Person(email:String, 
                    displayName:String)
      
  case class Time(date:String, 
                  dateTime:String, 
                  timeZone:Option[String])
    
  case class Attendee(email:String, 
                      displayName:Option[String], 
                      organizer:Boolean, 
                      self:Boolean, 
                      resource:Boolean, 
                      optional:Boolean, 
                      responseStatus:String, 
                      comment:Option[String], 
                      additionalGuests:Int)

  val extendedProperties = wrap(ExtendedProperties)(ExtendedProperties.unapply(_).get)
  val gadget             = wrap(Gadget)(Gadget.unapply(_).get)
  val person             = wrap(Person)(Person.unapply(_).get)
  val time               = wrap(Time)(Time.unapply(_).get)
  val attendee           = wrap(Attendee)(Attendee.unapply(_).get)
  
  
  ("kind"                   :: string("calendar#event")) ~
  ("etag"                   :: etag)             ~
  ("id"                     :: string)           ~
  ("status"                 :: string("confirmed", "tentative", "cancelled")).? ~
  ("htmlLink"               :: string)           ~
  ("created"                :: datetime)         ~
  ("updated"                :: datetime)         ~
  ("summary"                :: string)           ~
  ("description"            :: string).?         ~
  ("location"               :: string).?         ~
  ("colorId"                :: string).?         ~    
  ("creator"                :: person(
    ("email"                :: string)           ~
    ("displayName"          :: string)))         ~    
  ("organizer"              :: person( 
    ("email"                :: string)           ~
    ("displayName"          :: string)))         ~    
  ("start"                  :: time(
    ("date"                 :: date)             ~
    ("dateTime"             :: datetime)         ~
    ("timeZone"             :: string).?))       ~    
  ("end"                    :: time( 
    ("date"                 :: date)             ~
    ("dateTime"             :: datetime)         ~
    ("timeZone"             :: string).?))       ~    
  ("recurrence"             :: array(string))    ~    
  ("recurringEventId"       :: string)           ~    
  ("originalStartTime"      :: time(
    ("date"                 :: date)             ~
    ("dateTime"             :: datetime)         ~
    ("timeZone"             :: string).?))       ~    
  ("transparency"           :: string("opaque", "transparent")).? ~
  ("visibility"             :: string("default", "public", "private", "confidential")).? ~
  ("iCalUID"                :: string)           ~
  ("sequence"               :: int)          ~    
  ("attendees"              :: array(attendee(
    (("email"               :: string)           ~
    ("displayName"          :: string).?         ~
    ("organizer"            :: boolean)          ~
    ("self"                 :: boolean)          ~
    ("resource"             :: boolean)          ~
    ("optional"             :: boolean).?(false) ~
    ("responseStatus"       :: string("needsAction", "declined", "tentative", "accepted")) ~
    ("comment"              :: string).?         ~
    ("additionalGuests"     :: int).?(0))))) ~    
  ("attendeesOmitted"       :: boolean).?(false) ~    
  ("extendedProperties"     :: extendedProperties(
    ("private"              :: 
      (*                    :: string))          ~    
    ("shared"               :: 
      (*                    :: string))))        ~        
  ("gadget"                 :: gadget(
    ("type"                 :: string)           ~
    ("title"                :: string)           ~
    ("link"                 :: string)           ~
    ("iconLink"             :: string)           ~
    ("width"                :: int).?        ~
    ("height"               :: int).?        ~
    ("display"              :: string("icon", "chip")).? ~
    ("preferences"          :: 
      (*                    :: string))))        ~        
  ("anyoneCanAddSelf"       :: boolean).?(false) ~
  ("guestsCanInviteOthers"  :: boolean).?(false) ~
  ("guestsCanSeeOtherGuests":: boolean).?(false) ~
  ("privateCopy"            :: boolean).?        ~    
  ("reminders"              :: 
    ("useDefault"           :: boolean)          ~
    ("overrides"            :: array(
      ("method"             :: string("email", "sms", "popup")) ~
      ("minutes"            :: int))))  
}
