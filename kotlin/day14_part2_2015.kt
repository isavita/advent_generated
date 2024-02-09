import java.io.File

data class Reindeer(
    var speed: Int,
    var flyTime: Int,
    var restTime: Int,
    var distance: Int = 0,
    var points: Int = 0,
    var flying: Boolean = true,
    var timeInMode: Int = 0
)

fun main(args: Array<String>) {
    val reindeers = readReindeerDetails("input.txt")
    simulateRaceWithPoints(reindeers, 2503)
    val maxPoints = findMaxPoints(reindeers)
    println(maxPoints)
}

fun readReindeerDetails(filename: String): MutableList<Reindeer> {
    val reindeers = mutableListOf<Reindeer>()
    File(filename).forEachLine {
        val parts = it.split(" ")
        val speed = parts[3].toInt()
        val flyTime = parts[6].toInt()
        val restTime = parts[13].toInt()
        reindeers.add(Reindeer(speed, flyTime, restTime))
    }
    return reindeers
}

fun simulateRaceWithPoints(reindeers: MutableList<Reindeer>, totalSeconds: Int) {
    repeat(totalSeconds) {
        var maxDistance = 0
        for (reindeer in reindeers) {
            if (reindeer.flying) {
                reindeer.distance += reindeer.speed
            }
            reindeer.timeInMode++
            if ((reindeer.flying && reindeer.timeInMode == reindeer.flyTime) || (!reindeer.flying && reindeer.timeInMode == reindeer.restTime)) {
                reindeer.flying = !reindeer.flying
                reindeer.timeInMode = 0
            }
            if (reindeer.distance > maxDistance) {
                maxDistance = reindeer.distance
            }
        }
        for (reindeer in reindeers) {
            if (reindeer.distance == maxDistance) {
                reindeer.points++
            }
        }
    }
}

fun findMaxPoints(reindeers: List<Reindeer>): Int {
    var maxPoints = 0
    for (reindeer in reindeers) {
        if (reindeer.points > maxPoints) {
            maxPoints = reindeer.points
        }
    }
    return maxPoints
}