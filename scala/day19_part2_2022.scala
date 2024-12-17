
import scala.io.Source
import scala.util.Using

object Main {

  case class Blueprint(id: Int, oreCost: Int, clayOreCost: Int, obsidianOreCost: Int, obsidianClayCost: Int, geodeOreCost: Int, geodeObsidianCost: Int)
  case class State(ore: Int, clay: Int, obsidian: Int, geode: Int, oreRobots: Int, clayRobots: Int, obsidianRobots: Int, geodeRobots: Int, timeLeft: Int)

  def maxGeode(b: Blueprint, st: State): Int = {
    var max = 0
    var q = collection.mutable.Queue(st)
    val visited = collection.mutable.Set[State]()

    while (q.nonEmpty) {
      val s = q.dequeue()
      max = math.max(max, s.geode)
      if (s.timeLeft == 0) {
        
      } else {
        val o = math.max(math.max(b.oreCost, b.clayOreCost), math.max(b.obsidianOreCost, b.geodeOreCost))
        var oreRobots = s.oreRobots
        if (oreRobots >= o) {
          oreRobots = o
        }
        var clayRobots = s.clayRobots
        if (clayRobots >= b.obsidianClayCost) {
          clayRobots = b.obsidianClayCost
        }
        var obsidianRobots = s.obsidianRobots
        if (obsidianRobots >= b.geodeObsidianCost) {
          obsidianRobots = b.geodeObsidianCost
        }
        var ore = s.ore
        val maxOre = s.timeLeft * o - s.oreRobots * (s.timeLeft - 1)
        if (ore >= maxOre) {
          ore = maxOre
        }
        var clay = s.clay
        val maxClay = s.timeLeft * b.obsidianClayCost - s.clayRobots * (s.timeLeft - 1)
        if (clay >= maxClay) {
          clay = maxClay
        }
        var obsidian = s.obsidian
        val maxObsidian = s.timeLeft * b.geodeObsidianCost - s.obsidianRobots * (s.timeLeft - 1)
        if (obsidian >= maxObsidian) {
          obsidian = maxObsidian
        }
        val newState = State(ore, clay, obsidian, s.geode, oreRobots, clayRobots, obsidianRobots, s.geodeRobots, s.timeLeft)
        if (!visited.contains(newState)) {
          visited.add(newState)
          q.enqueue(State(s.ore + s.oreRobots, s.clay + s.clayRobots, s.obsidian + s.obsidianRobots, s.geode + s.geodeRobots, s.oreRobots, s.clayRobots, s.obsidianRobots, s.geodeRobots, s.timeLeft - 1))

          if (s.ore >= b.oreCost) {
            q.enqueue(State(s.ore - b.oreCost + s.oreRobots, s.clay + s.clayRobots, s.obsidian + s.obsidianRobots, s.geode + s.geodeRobots, s.oreRobots + 1, s.clayRobots, s.obsidianRobots, s.geodeRobots, s.timeLeft - 1))
          }

          if (s.ore >= b.clayOreCost) {
            q.enqueue(State(s.ore - b.clayOreCost + s.oreRobots, s.clay + s.clayRobots, s.obsidian + s.obsidianRobots, s.geode + s.geodeRobots, s.oreRobots, s.clayRobots + 1, s.obsidianRobots, s.geodeRobots, s.timeLeft - 1))
          }

          if (s.ore >= b.obsidianOreCost && s.clay >= b.obsidianClayCost) {
            q.enqueue(State(s.ore - b.obsidianOreCost + s.oreRobots, s.clay - b.obsidianClayCost + s.clayRobots, s.obsidian + s.obsidianRobots, s.geode + s.geodeRobots, s.oreRobots, s.clayRobots, s.obsidianRobots + 1, s.geodeRobots, s.timeLeft - 1))
          }

          if (s.ore >= b.geodeOreCost && s.obsidian >= b.geodeObsidianCost) {
            q.enqueue(State(s.ore - b.geodeOreCost + s.oreRobots, s.clay + s.clayRobots, s.obsidian - b.geodeObsidianCost + s.obsidianRobots, s.geode + s.geodeRobots, s.oreRobots, s.clayRobots, s.obsidianRobots, s.geodeRobots + 1, s.timeLeft - 1))
          }
        }
      }
    }
    max
  }

  def main(args: Array[String]): Unit = {
    val blueprints = Using(Source.fromFile("input.txt")) { source =>
      source.getLines().map { line =>
        val parts = line.split("[^0-9]+").filter(_.nonEmpty).map(_.toInt)
        Blueprint(parts(0), parts(1), parts(2), parts(3), parts(4), parts(5), parts(6))
      }.toList
    }.get

    val init = State(0, 0, 0, 0, 1, 0, 0, 0, 32)
    val prod = blueprints.take(3).map(b => maxGeode(b, init)).product
    println(prod)
  }
}
