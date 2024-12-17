
import scala.io.Source
import scala.util.Using
import scala.collection.mutable

object Main {
  def main(args: Array[String]): Unit = {
    val input = Using(Source.fromFile("input.txt"))(_.mkString).get
    println(part1(input.trim))
  }

  def part1(input: String): Int = {
    val blueprints = parseInput(input)
    blueprints.map { bp =>
      val st = new State(bp)
      bp.id * st.calcMostGeodes(0, mutable.Map.empty, 24, 24)
    }.sum
  }

  case class Blueprint(
      id: Int,
      oreForOreRobot: Int,
      oreForClayRobot: Int,
      oreForObsidianRobot: Int,
      clayForObsidianRobot: Int,
      oreForGeodeRobot: Int,
      obsidianForGeodeRobot: Int
  )

  class State(val blueprint: Blueprint) {
    var ore: Int = 0
    var clay: Int = 0
    var obsidian: Int = 0
    var geode: Int = 0
    var oreRobots: Int = 1
    var clayRobots: Int = 0
    var obsidianRobots: Int = 0
    var geodeRobots: Int = 0

    def farm(): Unit = {
      ore += oreRobots
      clay += clayRobots
      obsidian += obsidianRobots
      geode += geodeRobots
    }

    def hash(time: Int): String = {
      s"$time,$ore,$clay,$obsidian,$geode,$oreRobots,$clayRobots,$obsidianRobots,$geodeRobots"
    }

    def copy(): State = {
      val newState = new State(blueprint)
      newState.ore = ore
      newState.clay = clay
      newState.obsidian = obsidian
      newState.geode = geode
      newState.oreRobots = oreRobots
      newState.clayRobots = clayRobots
      newState.obsidianRobots = obsidianRobots
      newState.geodeRobots = geodeRobots
      newState
    }

    def calcMostGeodes(
        time: Int,
        memo: mutable.Map[String, Int],
        totalTime: Int,
        earliestGeode: Int
    ): Int = {
      if (time == totalTime) {
        return geode
      }

      val h = hash(time)
      if (memo.contains(h)) {
        return memo(h)
      }

      if (geode == 0 && time > earliestGeode) {
        return 0
      }

      var mostGeodes = geode

      if (ore >= blueprint.oreForGeodeRobot && obsidian >= blueprint.obsidianForGeodeRobot) {
        val cp = copy()
        cp.farm()
        cp.ore -= blueprint.oreForGeodeRobot
        cp.obsidian -= blueprint.obsidianForGeodeRobot
        cp.geodeRobots += 1
        val newEarliestGeode = if (cp.geodeRobots == 1) math.min(earliestGeode, time + 1) else earliestGeode
        mostGeodes = math.max(mostGeodes, cp.calcMostGeodes(time + 1, memo, totalTime, newEarliestGeode))
        memo(h) = mostGeodes
        return mostGeodes
      }

      if (time <= totalTime - 16 && oreRobots < blueprint.oreForObsidianRobot * 2 && ore >= blueprint.oreForOreRobot) {
        val cp = copy()
        cp.ore -= blueprint.oreForOreRobot
        cp.farm()
        cp.oreRobots += 1
        mostGeodes = math.max(mostGeodes, cp.calcMostGeodes(time + 1, memo, totalTime, earliestGeode))
      }
      if (time <= totalTime - 8 && clayRobots < blueprint.clayForObsidianRobot && ore >= blueprint.oreForClayRobot) {
        val cp = copy()
        cp.ore -= blueprint.oreForClayRobot
        cp.farm()
        cp.clayRobots += 1
        mostGeodes = math.max(mostGeodes, cp.calcMostGeodes(time + 1, memo, totalTime, earliestGeode))
      }
      if (time <= totalTime - 4 && obsidianRobots < blueprint.obsidianForGeodeRobot && ore >= blueprint.oreForObsidianRobot && clay >= blueprint.clayForObsidianRobot) {
        val cp = copy()
        cp.ore -= blueprint.oreForObsidianRobot
        cp.clay -= blueprint.clayForObsidianRobot
        cp.farm()
        cp.obsidianRobots += 1
        mostGeodes = math.max(mostGeodes, cp.calcMostGeodes(time + 1, memo, totalTime, earliestGeode))
      }

      val cp = copy()
      cp.farm()
      mostGeodes = math.max(mostGeodes, cp.calcMostGeodes(time + 1, memo, totalTime, earliestGeode))

      memo(h) = mostGeodes
      mostGeodes
    }
  }

  def parseInput(input: String): List[Blueprint] = {
    input.split("\n").map { line =>
      val parts = line.split(" ").filter(_.nonEmpty)
      Blueprint(
        parts(1).dropRight(1).toInt,
        parts(6).toInt,
        parts(12).toInt,
        parts(18).toInt,
        parts(21).toInt,
        parts(27).toInt,
        parts(30).toInt
      )
    }.toList
  }
}
