package timelog_parser

import java.time.format.{DateTimeFormatterBuilder, TextStyle}
import java.time.temporal.ChronoField
import java.util.Locale

/**
  * Created by arturas on 2017-03-05.
  */
object Formats {
  // 30 Sep 2015
  val DateFormat =
    new DateTimeFormatterBuilder()
      .appendValue(ChronoField.DAY_OF_MONTH)
      .appendLiteral(' ')
      .appendText(ChronoField.MONTH_OF_YEAR, TextStyle.SHORT)
      .appendLiteral(' ')
      .appendValue(ChronoField.YEAR)
      .toFormatter(Locale.ENGLISH)

  // Wednesday, September 30, 2015 12:33:08 PM +02:00
  val WorkEntryFormatOld =
    new DateTimeFormatterBuilder()
      .appendText(ChronoField.DAY_OF_WEEK, TextStyle.FULL)
      .appendLiteral(", ")
      .appendText(ChronoField.MONTH_OF_YEAR, TextStyle.FULL)
      .appendLiteral(' ')
      .appendValue(ChronoField.DAY_OF_MONTH)
      .appendLiteral(", ")
      .appendValue(ChronoField.YEAR)
      .appendLiteral(' ')
      .appendValue(ChronoField.CLOCK_HOUR_OF_AMPM)
      .appendLiteral(':')
      .appendValue(ChronoField.MINUTE_OF_HOUR)
      .appendLiteral(':')
      .appendValue(ChronoField.SECOND_OF_MINUTE)
      .appendLiteral(' ')
      .appendText(ChronoField.AMPM_OF_DAY)
      .appendLiteral(' ')
      .appendZoneOrOffsetId()
      .toFormatter(Locale.ENGLISH)

  // 2016-12-11 16:18:37 +0200
  val WorkEntryFormat =
    new DateTimeFormatterBuilder()
      .appendValue(ChronoField.YEAR)
      .appendLiteral("-")
      .appendValue(ChronoField.MONTH_OF_YEAR)
      .appendLiteral('-')
      .appendValue(ChronoField.DAY_OF_MONTH)
      .appendLiteral(" ")
      .appendValue(ChronoField.HOUR_OF_DAY)
      .appendLiteral(':')
      .appendValue(ChronoField.MINUTE_OF_HOUR)
      .appendLiteral(':')
      .appendValue(ChronoField.SECOND_OF_MINUTE)
      .appendLiteral(' ')
      .appendOffset("+HHMM", "UTC")
      .toFormatter(Locale.ENGLISH)

  val WorkEntryFormats = Vector(WorkEntryFormat, WorkEntryFormatOld)
}
