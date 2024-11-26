
import scala.io.Source
import scala.collection.mutable

object ReposeRecord {
  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").getLines().toList.sorted
    val guardSleepMinutes = processGuardRecords(input)

    // Part 1: Find guard with most total sleep time
    val (guardId1, maxMinute1) = findGuardMostAsleep(guardSleepMinutes)
    println(s"Part 1: ${guardId1 * maxMinute1}")

    // Part 2: Find guard most frequently asleep on same minute
    val (guardId2, maxMinute2) = findGuardMostFrequentlyAsleepOnSameMinute(guardSleepMinutes)
    println(s"Part 2: ${guardId2 * maxMinute2}")
  }

  def processGuardRecords(records: List[String]): mutable.Map[Int, Array[Int]] = {
    val guardSleepMinutes = mutable.Map[Int, Array[Int]]()
    var currentGuard = 0
    var sleepStart = 0

    records.foreach { record =>
      record match {
        case r if r.contains("Guard #") =>
          currentGuard = r.split("#")(1).split(" ")(0).toInt
          if (!guardSleepMinutes.contains(currentGuard)) {
            guardSleepMinutes(currentGuard) = Array.fill(60)(0)
          }
        case r if r.contains("falls asleep") =>
          sleepStart = r.split(":")(1).split("]")(0).toInt
        case r if r.contains("wakes up") =>
          val sleepEnd = r.split(":")(1).split("]")(0).toInt
          (sleepStart until sleepEnd).foreach { minute =>
            guardSleepMinutes(currentGuard)(minute) += 1
          }
      }
    }
    guardSleepMinutes
  }

  def findGuardMostAsleep(guardSleepMinutes: mutable.Map[Int, Array[Int]]): (Int, Int) = {
    val guardTotalSleep = guardSleepMinutes.map { case (guard, minutes) => 
      (guard, minutes.sum) 
    }
    val guardWithMostSleep = guardTotalSleep.maxBy(_._2)._1
    val maxMinute = guardSleepMinutes(guardWithMostSleep).zipWithIndex.maxBy(_._1)._2
    (guardWithMostSleep, maxMinute)
  }

  def findGuardMostFrequentlyAsleepOnSameMinute(guardSleepMinutes: mutable.Map[Int, Array[Int]]): (Int, Int) = {
    val minuteFrequency = guardSleepMinutes.map { case (guard, minutes) =>
      val (maxFreq, maxMinute) = minutes.zipWithIndex.maxBy(_._1)
      (guard, maxFreq, maxMinute)
    }
    val (guardId, _, maxMinute) = minuteFrequency.maxBy(_._2)
    (guardId, maxMinute)
  }
}
