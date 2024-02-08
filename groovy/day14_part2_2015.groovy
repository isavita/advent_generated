def reindeer = [:]
def maxDistance = 0
new File("input.txt").eachLine { line ->
    def parts = line.split(" ")
    def name = parts[0]
    def speed = parts[3] as int
    def flyTime = parts[6] as int
    def restTime = parts[13] as int
    reindeer[name] = [speed: speed, flyTime: flyTime, restTime: restTime, distance: 0, points: 0, flying: true, timeLeft: flyTime]
}

(1..2503).each { _ ->
    reindeer.each { k, v ->
        if (v.flying) {
            v.distance += v.speed
        }
        v.timeLeft--

        if (v.timeLeft == 0) {
            v.flying = !v.flying
            v.timeLeft = v.flying ? v.flyTime : v.restTime
        }

        maxDistance = Math.max(maxDistance, v.distance)
    }

    reindeer.findAll { it.value.distance == maxDistance }.each { it.value.points++ }
}

println reindeer.collect { k, v -> v.points }.max()