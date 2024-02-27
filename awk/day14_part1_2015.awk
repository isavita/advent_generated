
{
    speed = $4
    flyTime = $7
    restTime = $14
    distance = 0
    flying = 1
    timeInMode = 0
    reindeers[NR] = speed " " flyTime " " restTime " " distance " " flying " " timeInMode
}

END {
    simulateRace(reindeers, 2503)
    maxDistance = findMaxDistance(reindeers)
    print maxDistance
}

function simulateRace(reindeers, totalSeconds) {
    for (i = 1; i <= totalSeconds; i++) {
        for (j = 1; j <= length(reindeers); j++) {
            split(reindeers[j], reindeer, " ")
            if (reindeer[5] == 1) {
                reindeer[4] += reindeer[1]
                reindeer[6]++
                if (reindeer[6] == reindeer[2]) {
                    reindeer[5] = 0
                    reindeer[6] = 0
                }
            } else {
                reindeer[6]++
                if (reindeer[6] == reindeer[3]) {
                    reindeer[5] = 1
                    reindeer[6] = 0
                }
            }
            reindeers[j] = reindeer[1] " " reindeer[2] " " reindeer[3] " " reindeer[4] " " reindeer[5] " " reindeer[6]
        }
    }
}

function findMaxDistance(reindeers) {
    maxDistance = 0
    for (i = 1; i <= length(reindeers); i++) {
        split(reindeers[i], reindeer, " ")
        if (reindeer[4] > maxDistance) {
            maxDistance = reindeer[4]
        }
    }
    return maxDistance
}
