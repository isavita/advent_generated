
import java.io.File
import java.util.*
import kotlin.math.max

data class Blueprint(
    val id: Int,
    val oreCost: Int,
    val clayOreCost: Int,
    val obsidianOreCost: Int,
    val obsidianClayCost: Int,
    val geodeOreCost: Int,
    val geodeObsidianCost: Int
)

data class State(
    var ore: Int = 0,
    var clay: Int = 0,
    var obsidian: Int = 0,
    var geode: Int = 0,
    var oreRobots: Int = 1,
    var clayRobots: Int = 0,
    var obsidianRobots: Int = 0,
    var geodeRobots: Int = 0,
    var timeLeft: Int
)

fun main() {
    val blueprints = parseBlueprints()
    val init = State(timeLeft = 32)
    var prod = 1
    for (b in blueprints.take(3)) {
        prod *= maxGeode(b, init)
    }
    println(prod)
}

fun parseBlueprints(): List<Blueprint> {
    val blueprints = mutableListOf<Blueprint>()
    File("input.txt").forEachLine { line ->
        val parts = line.split(" ")
        blueprints.add(
            Blueprint(
                parts[1].removeSuffix(":").toInt(),
                parts[6].toInt(),
                parts[12].toInt(),
                parts[18].toInt(),
                parts[21].toInt(),
                parts[27].toInt(),
                parts[30].toInt()
            )
        )
    }
    return blueprints
}

fun maxGeode(b: Blueprint, st: State): Int {
    var maxGeodes = 0
    val queue: Queue<State> = LinkedList()
    val visited = mutableSetOf<State>()
    queue.offer(st)

    while (queue.isNotEmpty()) {
        val s = queue.poll()
        maxGeodes = max(maxGeodes, s.geode)
        if (s.timeLeft == 0) continue

        val maxOreCost = max(max(b.oreCost, b.clayOreCost), max(b.obsidianOreCost, b.geodeOreCost))
        if (s.oreRobots >= maxOreCost) s.oreRobots = maxOreCost
        if (s.clayRobots >= b.obsidianClayCost) s.clayRobots = b.obsidianClayCost
        if (s.obsidianRobots >= b.geodeObsidianCost) s.obsidianRobots = b.geodeObsidianCost

        val maxOre = s.timeLeft * maxOreCost - s.oreRobots * (s.timeLeft - 1)
        if (s.ore >= maxOre) s.ore = maxOre
        val maxClay = s.timeLeft * b.obsidianClayCost - s.clayRobots * (s.timeLeft - 1)
        if (s.clay >= maxClay) s.clay = maxClay
        val maxObsidian = s.timeLeft * b.geodeObsidianCost - s.obsidianRobots * (s.timeLeft - 1)
        if (s.obsidian >= maxObsidian) s.obsidian = maxObsidian

        if (s in visited) continue
        visited.add(s.copy())

        queue.offer(s.copy().apply {
            ore += oreRobots
            clay += clayRobots
            obsidian += obsidianRobots
            geode += geodeRobots
            timeLeft--
        })

        if (s.ore >= b.oreCost) {
            queue.offer(s.copy().apply {
                ore = ore - b.oreCost + oreRobots
                clay += clayRobots
                obsidian += obsidianRobots
                geode += geodeRobots
                oreRobots++
                timeLeft--
            })
        }

        if (s.ore >= b.clayOreCost) {
            queue.offer(s.copy().apply {
                ore = ore - b.clayOreCost + oreRobots
                clay += clayRobots
                obsidian += obsidianRobots
                geode += geodeRobots
                clayRobots++
                timeLeft--
            })
        }

        if (s.ore >= b.obsidianOreCost && s.clay >= b.obsidianClayCost) {
            queue.offer(s.copy().apply {
                ore = ore - b.obsidianOreCost + oreRobots
                clay = clay - b.obsidianClayCost + clayRobots
                obsidian += obsidianRobots
                geode += geodeRobots
                obsidianRobots++
                timeLeft--
            })
        }

        if (s.ore >= b.geodeOreCost && s.obsidian >= b.geodeObsidianCost) {
            queue.offer(s.copy().apply {
                ore = ore - b.geodeOreCost + oreRobots
                clay += clayRobots
                obsidian = obsidian - b.geodeObsidianCost + obsidianRobots
                geode += geodeRobots
                geodeRobots++
                timeLeft--
            })
        }
    }
    return maxGeodes
}
