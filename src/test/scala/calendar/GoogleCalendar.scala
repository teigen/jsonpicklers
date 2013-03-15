package calendar

import org.scalatest.PropSpec
import org.json4s.native.JsonParser
import jsonpicklers._

class GoogleCalendar extends PropSpec {
  
  property("calendarList"){
    import CalendarList._
    
    val source = JsonParser.parse(
"""
{
  "kind": "calendar#calendarListEntry",
  "etag": "etag",
  "id": "string",
  "summary": "string",
  "description": "string",
  "location": "string",
  "summaryOverride": "string",
  "colorId": "string",
  "hidden": true,
  "selected" : false,
  "accessRole": "freeBusyReader",
  "defaultReminders": [
    {
      "method": "email",
      "minutes": 5
    }
  ]
}
""")
    
    val unpickled = calendarList.unpickle(source).get
    val expected = CalendarList("calendar#calendarListEntry","etag","string","string",Some("string"),Some("string"),None,Some("string"),Some("string"),true,false,"freeBusyReader",List(Reminder("email",5)))
    
    assert(unpickled === expected)
    
    assert(calendarList.pickle(expected) === Some(source))
  }
}
