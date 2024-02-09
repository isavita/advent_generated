import java.io.File

data class Reindeer(val name: String, val speed: Int, val flyTime: Int, val restTime: Int)

fun main() {
    val reindeers = mutableListOf<Reindeer>()
    File("input.txt").readLines().forEach {
        val parts = it.split(" ")
        val name = parts[0]
        val speed = parts[3].toInt()
        val flyTime = parts[6].toInt()
        val restTime = parts[13].toInt()
        reindeers.add(Reindeer(name, speed, flyTime, restTime))
    }

    val raceTime = 2503
    var maxDistance = 0
    for (reindeer in reindeers) {
        val cycleTime = reindeer.flyTime + reindeer.restTime
        val fullCycles = raceTime / cycleTime
        val remainingTime = raceTime % cycleTime
        val distance = fullCycles * reindeer.speed * reindeer.flyTime + 
                       minOf(reindeer.flyTime, remainingTime) * reindeer.speed
        if (distance > maxDistance) {
            maxDistance = distance
        }
    }

    println(maxDistance)
}