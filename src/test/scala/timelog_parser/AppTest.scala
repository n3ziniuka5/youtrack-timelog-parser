package timelog_parser

import org.joda.time.chrono.ISOChronology
import org.joda.time.{Chronology, DateTime, DateTimeZone}
import org.specs2.mutable.Specification
import timelog_parser.WorkEntry.ExactTime

class AppTest extends Specification {
  def splitLines(s: String): Array[String] = s.split("[\r\n]+")

  val dt = new DateTime(
    2016, 11, 29, 13, 11, 16, DateTimeZone.forOffsetHours(2)
  )
  App.WorkEntryFormats.foreach { fmt =>
    s"format ${dt.toString(fmt)}" >> {
      "must be serializable and deserializable" >> {
        val dts = dt.toString(fmt)
        val parsed = DateTime.parse(dts, fmt).withChronology(dt.getChronology)
        // Chronologies differ, parsed one has Europe/Helsinki
        parsed must_=== dt
      }
    }
  }

  "old style should parse" >> {
    val lines = splitLines(
s"""
Andrius Dapševičius
Andrius Dapševičius
29 Nov 2016
Work: Tuesday, November 29, 2016 1:11:16 PM +02:00 - Tuesday, November 29, 2016 5:31:03 PM +02:00
4 hours 19 min
No type
 """)
    val result = App.process(lines)
    val chronology = ISOChronology.getInstance(DateTimeZone.forID("Europe/Helsinki"))
    val expected = Vector(ExactTime(WorkflowDateRange(
      new DateTime(2016, 11, 29, 13, 11, 16, chronology),
      new DateTime(2016, 11, 29, 17, 31, 3, chronology)
    )))
    result must_=== expected
  }

  "new style should parse" >> {
    val lines = splitLines(
s"""
Artūras Šlajus
Artūras Šlajus
11 Dec 2016
Work: 2016-12-11 16:18:37 +0200 to 2016-12-11 16:18:40 +0200
No type
""")
    val result = App.process(lines)
    val chronology = ISOChronology.getInstance(DateTimeZone.forID("Europe/Helsinki"))
    val expected = Vector(ExactTime(WorkflowDateRange(
      new DateTime(2016, 12, 11, 16, 18, 37, chronology),
      new DateTime(2016, 12, 11, 16, 18, 40, chronology)
    )))
    result must_=== expected
  }
}