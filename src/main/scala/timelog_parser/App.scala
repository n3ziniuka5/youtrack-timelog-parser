package timelog_parser

import java.util.concurrent.TimeUnit

import org.joda.time.DateTime
import org.joda.time.format.{DateTimeFormatterBuilder, DateTimeFormatter}

import scalaz._, Scalaz._
import scalaz.effect._
import scala.concurrent.duration._
import scalaz.stream._

case class WorkflowDateRange(start: DateTime, end: DateTime) {
  def toDuration = FiniteDuration(end.getMillis - start.getMillis, TimeUnit.MILLISECONDS)
}

sealed trait WorkEntry {
  def date: DateTime
  def duration: FiniteDuration
}
object WorkEntry {
  case class ExactTime(range: WorkflowDateRange) extends WorkEntry {
    override def date = range.start
    override def duration = range.toDuration
  }
  case class YouTrack(date: DateTime, duration: FiniteDuration) extends WorkEntry
}

object App extends SafeApp {
  // 30 Sep 2015
  val DateFormat = new DateTimeFormatterBuilder()
    .appendDayOfMonth(1)
    .appendLiteral(' ')
    .appendMonthOfYearText()
    .appendLiteral(' ')
    .appendYear(4, 4)
    .toFormatter

  // Wednesday, September 30, 2015 12:33:08 PM +02:00
  val WorkEntryFormat = new DateTimeFormatterBuilder()
    .appendDayOfWeekText()
    .appendLiteral(", ")
    .appendMonthOfYearText()
    .appendLiteral(' ')
    .appendDayOfMonth(1)
    .appendLiteral(", ")
    .appendYear(4, 4)
    .appendLiteral(' ')
    .appendClockhourOfHalfday(1)
    .appendLiteral(':')
    .appendMinuteOfHour(1)
    .appendLiteral(':')
    .appendSecondOfMinute(1)
    .appendLiteral(' ')
    .appendHalfdayOfDayText()
    .appendLiteral(' ')
    .appendTimeZoneOffset(null, true, 2, 2)
    .toFormatter

  val DateRe = """^\d{2} \w{3} \d{4}$""".r
  val TimeRe = """^((\d+) hours? ?)?((\d+) min)?$""".r
  val WorkRe = """^Work: (.+?) - (.+)$""".r

  override def run(args: ImmutableArray[String]) = {
    for {
      lines <- if (args.isEmpty) readStdin else readFile(args(0))
      workEntries = process(lines)
      _ <- workEntries.map(e => IO.putStrLn(e.toString)).sequence_
      perDay = workedPerDay(workEntries)
      outLines = monthWorkLines(perDay)
      _ <- IO.putStrLn("===========")
      _ <- outLines.map(IO.putStrLn).sequence_
    } yield ()
  }

  def readSource(read: IO[String]): IO[Vector[String]] = {
    def rec(stream: Vector[String]): IO[Vector[String]] = read.flatMap {
      case "!done" => IO(stream)
      case line => rec(stream :+ line.trim)
    }

    rec(Vector.empty)
  }

  val readStdin = readSource(IO.readLn)
  def readFile(path: String) = IO(io.linesR(path).map(_.trim).runLog.run)

  def process(lines: IndexedSeq[String]): Vector[WorkEntry] = {
    def onLine(
      lines: IndexedSeq[String], current: Vector[WorkEntry]
    )(
      f: PartialFunction[(String, IndexedSeq[String]), Vector[WorkEntry]]
    ): Vector[WorkEntry] = {
      if (lines.isEmpty) current
      else {
        val line = lines.head
        val rest = lines.tail
        f.lift((line, rest)) match {
          case None => onLine(rest, current)(f)
          case Some(entries) => entries
        }
      }
    }

    def starting(
      lines: IndexedSeq[String], current: Vector[WorkEntry]
    ): Vector[WorkEntry] = {
      onLine(lines, current) {
        case (line @ DateRe(), rest) =>
          val date = DateTime.parse(line, DateFormat)
          hasDate(date, rest, current)
      }
    }

    def hasDate(
      date: DateTime, lines: IndexedSeq[String], current: Vector[WorkEntry]
    ): Vector[WorkEntry] = {
      onLine(lines, current) {
        case (line @ WorkRe(start, end), rest) =>
          val range = WorkflowDateRange(
            DateTime.parse(start, WorkEntryFormat), DateTime.parse(end, WorkEntryFormat)
          )
          val entry = WorkEntry.ExactTime(range)
          starting(rest, current :+ entry)
        case (line @ TimeRe(_, hoursS, _, minutesS), rest) =>
          val duration =
            FiniteDuration(hoursS.toInt, TimeUnit.HOURS) +
              FiniteDuration(minutesS.toInt, TimeUnit.MINUTES)
          val entry = WorkEntry.YouTrack(date, duration)
          starting(rest, current :+ entry)
      }
    }

    starting(lines, Vector.empty)
  }

  def workedPerDay(entries: Vector[WorkEntry]): Map[Int, FiniteDuration] =
    entries.groupBy(_.date.getDayOfMonth).mapValues(_.map(_.duration).reduce(_ + _))

  def monthWorkLines(perDay: Map[Int, FiniteDuration]): Vector[String] =
    (1 to 31).map { day =>
      perDay.get(day).fold("")(durationToSumString)
    }(collection.breakOut)

  def durationToSumString(duration: FiniteDuration): String = {
    val seconds = duration.toSeconds
    val hours = seconds / 60 / 60
    val minutes = (seconds - hours * 60 * 60) / 60
    val sum = hours + minutes.toDouble / 60
    "%.2f".formatLocal(java.util.Locale.US, sum)
  }
}