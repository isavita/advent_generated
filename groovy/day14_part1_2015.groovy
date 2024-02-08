
def reindeers = []

new File("input.txt").eachLine { line ->
    def parts = line.split()
    def speed = parts[3] as Integer
    def flyTime = parts[6] as Integer
    def restTime = parts[13] as Integer

    reindeers.add([speed: speed, flyTime: flyTime, restTime: restTime, distance: 0, flying: true, timeInMode: 0])
}

def simulateRace(def reindeers, def totalSeconds) {
    (0..totalSeconds-1).each { _ ->
        reindeers.each { reindeer ->
            if (reindeer.flying) {
                reindeer.distance += reindeer.speed
                reindeer.timeInMode++
                if (reindeer.timeInMode == reindeer.flyTime) {
                    reindeer.flying = false
                    reindeer.timeInMode = 0
                }
            } else {
                reindeer.timeInMode++
                if (reindeer.timeInMode == reindeer.restTime) {
                    reindeer.flying = true
                    reindeer.timeInMode = 0
                }
            }
        }
    }
}

simulateRace(reindeers, 2503)

def maxDistance = reindeers.max { it.distance }.distance
println maxDistance
