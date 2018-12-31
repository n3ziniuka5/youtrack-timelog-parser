package timelog_parser

import java.time._
import scalaz.std.vector._, scalaz.syntax.validation._

import org.specs2.mutable.Specification
import timelog_parser.WorkEntry.ExactTime

class AppTest extends Specification {
  def splitLines(s: String): Vector[String] = s.split("[\r\n]+").toVector

  {
    val dates = Vector(LocalDate.of(2016, 11, 29), LocalDate.of(2017, 1, 2))
    val times = Vector(LocalTime.of(13, 11, 16), LocalTime.of(1, 2, 3))
    val offsets = Vector(ZoneOffset.ofHours(2), ZoneOffset.ofHours(1), ZoneOffset.UTC)
    val zonedDateTimes = for {
      date <- dates
      time <- times
      offset <- offsets
    } yield ZonedDateTime.of(date, time, offset)

    zonedDateTimes.foreach { dt =>
      Formats.WorkEntryFormats.foreach { fmt =>
        val fmtDt = fmt.format(dt)
        s"format $fmtDt" >> {
          "must be serializable and deserializable" >> {
            val parsed = ZonedDateTime.parse(fmtDt, fmt)
            parsed must_=== dt
          }
        }
      }
    }
  }

  def date(
    year: Int, month: Int, day: Int, hour: Int, minute: Int, second: Int, offset: Int
  ): ZonedDateTime =
    ZonedDateTime.of(
      LocalDate.of(year, month, day), LocalTime.of(hour, minute, second),
      ZoneOffset.ofHours(offset)
    )

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
    val expected = Vector(ExactTime(WorkflowDateRange(
      date(2016, 11, 29, 13, 11, 16, 2),
      date(2016, 11, 29, 17, 31, 3, 2)
    ))).successNel[String]
    result must_=== expected
  }

  "new style should parse" >> {
    val lines = splitLines(
s"""
Artūras Šlajus
Artūras Šlajus
11 Dec 2016
Work: 2016-12-11 16:18:37 +0200 to 2016-12-11 16:18:40 +0200
3 hours 39 min
No type
""")
    val result = App.process(lines)
    val expected = Vector(ExactTime(WorkflowDateRange(
      date(2016, 12, 11, 16, 18, 37, 2),
      date(2016, 12, 11, 16, 18, 40, 2)
    ))).successNel[String]
    result must_=== expected
  }

  "new style (short months and days) should parse" >> {
    val lines = splitLines(
s"""
Artūras Šlajus
Artūras Šlajus
11 Dec 2016
Work: 2017-2-3 16:17:28 +0100 to 2017-2-3 16:18:40 +0100
3 hours 39 min
No type
""")
    val result = App.process(lines)
    val expected = Vector(ExactTime(WorkflowDateRange(
      date(2017, 2, 3, 16, 17, 28, 1),
      date(2017, 2, 3, 16, 18, 40, 1)
    ))).successNel[String]
    result must_=== expected
  }

  "new style (short months and days, UTC) should parse" >> {
    val lines = splitLines(
s"""
Artūras Šlajus
Artūras Šlajus
11 Dec 2016
Work: 2017-2-3 16:17:28 +0000 to 2017-2-3 16:18:40 UTC
3 hours 39 min
No type
""")
    val result = App.process(lines)
    val expected = Vector(ExactTime(WorkflowDateRange(
      date(2017, 2, 3, 16, 17, 28, 0),
      date(2017, 2, 3, 16, 18, 40, 0)
    ))).successNel[String]
    result must_=== expected
  }

  "new style (2018 Oct youtrack redesign, just now) should parse" >> {
    val lines = splitLines(
      s"""
Laurynas Tretjakovas  added spent time just now
 Work: 2018-11-1 09:17:50 +0200 to 2018-11-1 11:59:30 +0200
No type: 2 hours 41 minutes on 2018-11-01
""")
    val result = App.process(lines)
    val expected = Vector(ExactTime(WorkflowDateRange(
      date(2018, 11, 1, 9, 17, 50, 2),
      date(2018, 11, 1, 11, 59, 30, 2),
    ))).successNel[String]
    result must_=== expected
  }

  "new style (2018 Oct youtrack redesign, x minutes ago) should parse" >> {
    val lines = splitLines(
      s"""
Laurynas Tretjakovas  added spent time 2 minutes ago
 Work: 2018-11-1 09:17:50 +0200 to 2018-11-1 11:59:30 +0200
No type: 2 hours 41 minutes on 2018-11-01
""")
    val result = App.process(lines)
    val expected = Vector(ExactTime(WorkflowDateRange(
      date(2018, 11, 1, 9, 17, 50, 2),
      date(2018, 11, 1, 11, 59, 30, 2),
    ))).successNel[String]
    result must_=== expected
  }

  "new style (2018 Oct youtrack redesign, name and date time) should parse" >> {
    val lines = splitLines(
      s"""
Laurynas Tretjakovas  added spent time 2018-12-28T18:13:04
 Work: 2018-12-28 09:55:36 +0200 to 2018-12-28 18:13:04 +0200
No type: 8 hours 17 minutes on 2018-12-28
""")
    val result = App.process(lines)
    val expected = Vector(ExactTime(WorkflowDateRange(
      date(2018, 12, 28, 9, 55, 36, 2),
      date(2018, 12, 28, 18, 13, 4, 2),
    ))).successNel[String]
    result must_=== expected
  }

  "new style (2018 Oct youtrack redesign, x hours ago) should parse" >> {
    val lines = splitLines(
      s"""
17 hours ago
 Work: 2018-10-31 17:01:48 +0200 to 2018-10-31 18:43:23 +0200
No type: 1 hour 41 minutes on 2018-10-31
""")
    val result = App.process(lines)
    val expected = Vector(ExactTime(WorkflowDateRange(
      date(2018, 10, 31, 17, 1, 48, 2),
      date(2018, 10, 31, 18, 43, 23, 2),
    ))).successNel[String]
    result must_=== expected
  }

  "new style (2018 Oct youtrack redesign, date time) should parse" >> {
    val lines = splitLines(
      s"""
2018-10-30T20:54:13
 Work: 2018-10-30 18:19:29 +0200 to 2018-10-30 20:54:13 +0200
No type: 2 hours 34 minutes on 2018-10-30
""")
    val result = App.process(lines)
    val expected = Vector(ExactTime(WorkflowDateRange(
      date(2018, 10, 30, 18, 19, 29, 2),
      date(2018, 10, 30, 20, 54, 13, 2),
    ))).successNel[String]
    result must_=== expected
  }
}