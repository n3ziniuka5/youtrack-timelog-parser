package timelog_parser

import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
import java.time.{LocalDate, ZonedDateTime}
import java.util.concurrent.TimeUnit

import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.util.Try
import scalaz.Scalaz._
import scalaz._
import scalaz.effect._
import scalaz.stream._

object WorkflowDateRange {
  def create(start: ZonedDateTime, end: ZonedDateTime): String \/ WorkflowDateRange = {
    if (end isAfter start) new WorkflowDateRange(start, end).right
    else s"WorkflowDateRange cannot start ($start) after end ($end)!".left
  }
}
case class WorkflowDateRange private (start: ZonedDateTime, end: ZonedDateTime) {
  def toDuration = FiniteDuration(
    ChronoUnit.MILLIS.between(start, end), TimeUnit.MILLISECONDS
  )

  override def toString = s"WorkflowDateRange(${App.durationToSumString(toDuration)}h, $start - $end)"
}

sealed trait WorkEntry {
  def date: LocalDate
  def duration: FiniteDuration
}
object WorkEntry {
  case class ExactTime(range: WorkflowDateRange) extends WorkEntry {
    override def date = range.start.toLocalDate
    override def duration = range.toDuration

    override def toString = s"ExactTime(${App.durationToSumString(duration)}h, ${range.start} - ${range.end})"
  }
  case class YouTrack(date: LocalDate, duration: FiniteDuration) extends WorkEntry {
    override def toString = s"YouTrack (${App.durationToSumString(duration)}h, $date)"
  }
}

object App extends SafeApp {
  val DateRe = """^\d{2} \w{3} \d{4}$""".r
  val TimeRe = """^((\d+) hours? ?)?((\d+) min)?$""".r
  val WorkRe = """^Work: (.+?) (?:-|to) (.+)$""".r

  override def run(args: ImmutableArray[String]) = {
    for {
      lines <-
        if (args.isEmpty)
          IO.putStrLn("Waiting for input, !done\\n when finished\n")
            .flatMap(_ => readStdin)
        else
          readFile(args(0))
      workEntries = process(lines)
      _ <- workEntries.map(e => IO.putStrLn(e.toString)).sequence_
      perDay = workedPerDay(workEntries)
      workLines = groupedWorkLines(perDay)
      sumLine = workDurationSumLine(perDay)
      _ <- IO.putStrLn("===========")
      _ <- workLines.map(IO.putStrLn).sequence_
      _ <- IO.putStrLn("===========")
      _ <- IO.putStrLn(sumLine)
    } yield ()
  }

  def readSource(read: IO[String]): IO[Vector[String]] = {
    def rec(stream: Vector[String]): IO[Vector[String]] = read.flatMap {
      case "!done" | null => IO(stream)
      case line => rec(stream :+ line.trim)
    }

    rec(Vector.empty)
  }

  val readStdin = readSource(IO.readLn)
  def readFile(path: String) = IO(io.linesR(path).map(_.trim).runLog.run)

  def parseDate(s: String): ZonedDateTime =
    Formats.WorkEntryFormats.foldLeft(Option.empty[ZonedDateTime]) {
      case (None, format) =>
        Try { ZonedDateTime.parse(s, format) }.toOption
      case (o, _) => o
    }.getOrElse {
      throw new IllegalArgumentException(
        s"Can't parse $s as date time with ${Formats.WorkEntryFormats}"
      )
    }

  /* This would be nice if JVM had tail call optimizations. */
  def process(lines: IndexedSeq[String]): Vector[WorkEntry] = {
    @tailrec def onLine(
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
          val date = LocalDate.parse(line, Formats.DateFormat)
          hasDate(date, rest, current)
      }
    }

    def hasDate(
      date: LocalDate, lines: IndexedSeq[String], current: Vector[WorkEntry]
    ): Vector[WorkEntry] = {
      onLine(lines, current) {
        case (line @ WorkRe(start, end), rest) =>
          val entry = WorkflowDateRange.create(
            parseDate(start), parseDate(end)
          ).fold(
            err => sys.error(s"Error while parsing '$line' as work: $err"),
            range => WorkEntry.ExactTime(range)
          )
          starting(rest, current :+ entry)
        case (line @ TimeRe(_, hoursS, _, minutesS), rest) =>
          def parseInt(s: String, name: String) =
            if (s == null) 0.success
            else s.parseInt.leftMap { err =>
              s"Error while parsing '$s' as $name: $err"
            }

          val durationV = (
            parseInt(hoursS, "hours").toValidationNel |@|
            parseInt(minutesS, "minutes").toValidationNel
          ) { (hours, minutes) =>
            FiniteDuration(hours, TimeUnit.HOURS) +
            FiniteDuration(minutes, TimeUnit.MINUTES)
          }

          val duration = durationV.fold(
            errs => sys.error(s"Error while parsing '$line' as time: $errs"),
            identity
          )
          val entry = WorkEntry.YouTrack(date, duration)
          starting(rest, current :+ entry)
      }
    }

    starting(lines, Vector.empty)
  }

  def workedPerDay(entries: Vector[WorkEntry]): Map[String, FiniteDuration] = {
    val dateFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd")
    entries.groupBy(entry => entry.date.format(dateFormat)).mapValues(_.map(_.duration).reduce(_ + _))
  }

  def groupedWorkLines(perDay: Map[String, FiniteDuration]): Vector[String] =
    perDay.map[String, Vector[String]] { case (day, duration) =>
      s"$day - ${durationToSumString(duration)}"
    }(collection.breakOut).sorted

  def workDurationSumLine(perDay: Map[String, FiniteDuration]): String = {
    val total = perDay.values.reduce(_ + _)
    s"total hours = ${durationToSumString(total)}"
  }

  def durationToSumString(duration: FiniteDuration): String = {
    val seconds = duration.toSeconds
    val hours = seconds / 60 / 60
    val minutes = (seconds - hours * 60 * 60) / 60
    val sum = hours + minutes.toDouble / 60
    "%.2f".formatLocal(java.util.Locale.US, sum)
  }
}
