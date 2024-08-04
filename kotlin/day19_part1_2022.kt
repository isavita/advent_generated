import java.io.File

data class Blueprint(
    val id: Int,
    val oreForOreRobot: Int,
    val oreForClayRobot: Int,
    val oreForObsidianRobot: Int,
    val clayForObsidianRobot: Int,
    val oreForGeodeRobot: Int,
    val obsidianForGeodeRobot: Int
)

data class State(
    val blueprint: Blueprint,
    var ore: Int,
    var clay: Int,
    var obsidian: Int,
    var geode: Int,
    var oreRobots: Int = 1,
    var clayRobots: Int = 0,
    var obsidianRobots: Int = 0,
    var geodeRobots: Int = 0
) {
    fun farm() {
        ore += oreRobots
        clay += clayRobots
        obsidian += obsidianRobots
        geode += geodeRobots
    }

    fun hash(time: Int): String = "$time$ore$clay$obsidian$geode$oreRobots$clayRobots$obsidianRobots$geodeRobots"

    fun copy(): State = State(blueprint, ore, clay, obsidian, geode, oreRobots, clayRobots, obsidianRobots, geodeRobots)

    fun calcMostGeodes(time: Int, memo: MutableMap<String, Int>, totalTime: Int, earliestGeode: Int): Int {
        if (time == totalTime) return geode
        val h = hash(time)
        memo[h]?.let { return it }
        if (geode == 0 && time > earliestGeode) return 0

        var mostGeodes = geode
        if (ore >= blueprint.oreForGeodeRobot && obsidian >= blueprint.obsidianForGeodeRobot) {
            val cp = copy().apply {
                farm()
                ore -= blueprint.oreForGeodeRobot
                obsidian -= blueprint.obsidianForGeodeRobot
                geodeRobots++
            }
            mostGeodes = maxOf(mostGeodes, cp.calcMostGeodes(time + 1, memo, totalTime, minOf(earliestGeode, time + 1)))
        }
        if (time <= totalTime - 16 && oreRobots < blueprint.oreForObsidianRobot * 2 && ore >= blueprint.oreForOreRobot) {
            val cp = copy().apply {
                ore -= blueprint.oreForOreRobot
                farm()
                oreRobots++
            }
            mostGeodes = maxOf(mostGeodes, cp.calcMostGeodes(time + 1, memo, totalTime, earliestGeode))
        }
        if (time <= totalTime - 8 && clayRobots < blueprint.clayForObsidianRobot && ore >= blueprint.oreForClayRobot) {
            val cp = copy().apply {
                ore -= blueprint.oreForClayRobot
                farm()
                clayRobots++
            }
            mostGeodes = maxOf(mostGeodes, cp.calcMostGeodes(time + 1, memo, totalTime, earliestGeode))
        }
        if (time <= totalTime - 4 && obsidianRobots < blueprint.obsidianForGeodeRobot && ore >= blueprint.oreForObsidianRobot && clay >= blueprint.clayForObsidianRobot) {
            val cp = copy().apply {
                ore -= blueprint.oreForObsidianRobot
                clay -= blueprint.clayForObsidianRobot
                farm()
                obsidianRobots++
            }
            mostGeodes = maxOf(mostGeodes, cp.calcMostGeodes(time + 1, memo, totalTime, earliestGeode))
        }
        val cp = copy().apply { farm() }
        mostGeodes = maxOf(mostGeodes, cp.calcMostGeodes(time + 1, memo, totalTime, earliestGeode))

        memo[h] = mostGeodes
        return mostGeodes
    }
}

fun parseInput(input: String): List<Blueprint> =
    input.lines().map { line ->
        val (id, oreForOreRobot, oreForClayRobot, oreForObsidianRobot, clayForObsidianRobot, oreForGeodeRobot, obsidianForGeodeRobot) = 
            Regex("Blueprint (\\d+): Each ore robot costs (\\d+) ore. Each clay robot costs (\\d+) ore. Each obsidian robot costs (\\d+) ore and (\\d+) clay. Each geode robot costs (\\d+) ore and (\\d+) obsidian.")
            .find(line)!!.destructured
        Blueprint(id.toInt(), oreForOreRobot.toInt(), oreForClayRobot.toInt(), oreForObsidianRobot.toInt(), clayForObsidianRobot.toInt(), oreForGeodeRobot.toInt(), obsidianForGeodeRobot.toInt())
    }

fun main() {
    val input = File("input.txt").readText().trim()
    val blueprints = parseInput(input)
    val sum = blueprints.sumOf { bp ->
        val st = State(bp, 0, 0, 0, 0)
        st.calcMostGeodes(0, mutableMapOf(), 24, 24) * bp.id
    }
    println(sum)
}