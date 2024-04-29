import scala.io.Source
import scala.util.Try
import java.time._
import java.time.format.DateTimeFormatter

object Main {
  case class Record(timestamp: LocalDateTime, action: String, guardID: Int)

  def main(args: Array[String]): Unit = {
    val records = readAndParseInput("input.txt")
    val sortedRecords = records.sortBy(_.timestamp)

    var currentGuardID = 0
    var sleepStart: LocalDateTime = null
    val guardSleepMinutes = scala.collection.mutable.Map[Int, Array[Int]]()

    for (record <- sortedRecords) {
      record.action match {
        case "begins shift" => currentGuardID = record.guardID
        case "falls asleep" => sleepStart = record.timestamp
        case "wakes up" =>
          if (!guardSleepMinutes.contains(currentGuardID)) {
            guardSleepMinutes(currentGuardID) = new Array[Int](60)
          }
          for (i <- sleepStart.getMinute to record.timestamp.getMinute - 1) {
            guardSleepMinutes(currentGuardID)(i) += 1
          }
      }
    }

    var maxSleep = 0
    var sleepiestGuard = 0
    for ((guardID, minutes) <- guardSleepMinutes) {
      val totalSleep = minutes.sum
      if (totalSleep > maxSleep) {
        maxSleep = totalSleep
        sleepiestGuard = guardID
      }
    }

    var maxMinute = 0
    var maxMinuteCount = 0
    for (i <- guardSleepMinutes(sleepiestGuard).indices) {
      if (guardSleepMinutes(sleepiestGuard)(i) > maxMinuteCount) {
        maxMinuteCount = guardSleepMinutes(sleepiestGuard)(i)
        maxMinute = i
      }
    }

    println(sleepiestGuard * maxMinute)
  }

  def readAndParseInput(filename: String): Array[Record] = {
    val lines = Source.fromFile(filename).getLines().toArray
    val records = scala.collection.mutable.ArrayBuffer[Record]()

    for (line <- lines) {
      val parts = line.split("\\] ")
      val timePart = parts(0).substring(1)
      val actionPart = parts(1)

      val ts = LocalDateTime.parse(timePart, DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm"))
      var guardID = -1
      var action = actionPart

      if (actionPart.contains("Guard")) {
        val guardPattern = "Guard #(\\d+) begins shift".r
        guardPattern.findFirstMatchIn(actionPart) foreach { m =>
          guardID = m.group(1).toInt
        }
        action = "begins shift"
      } else if (actionPart.contains("falls asleep")) {
        action = "falls asleep"
      } else if (actionPart.contains("wakes up")) {
        action = "wakes up"
      }

      records += Record(ts, action, guardID)
    }

    records.toArray
  }
}