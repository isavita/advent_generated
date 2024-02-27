
{
    speed = $4
    flyTime = $7
    restTime = $14
    distance = 0
    points = 0
    flying = 1
    timeInMode = 0
    reindeers[NR] = speed " " flyTime " " restTime " " distance " " points " " flying " " timeInMode
}

END {
    simulateRaceWithPoints(reindeers, 2503)
    maxPoints = findMaxPoints(reindeers)
    print maxPoints
}

function simulateRaceWithPoints(reindeers, totalSeconds) {
    for (i = 0; i < totalSeconds; i++) {
        maxDistance = 0
        for (j = 1; j <= length(reindeers); j++) {
            split(reindeers[j], reindeer, " ")
            if (reindeer[6] == 1) {
                reindeer[4] += reindeer[1]
            }
            reindeer[7]++
            if ((reindeer[6] == 1 && reindeer[7] == reindeer[2]) || (reindeer[6] == 0 && reindeer[7] == reindeer[3])) {
                reindeer[6] = !reindeer[6]
                reindeer[7] = 0
            }
            if (reindeer[4] > maxDistance) {
                maxDistance = reindeer[4]
            }
            reindeers[j] = reindeer[1] " " reindeer[2] " " reindeer[3] " " reindeer[4] " " reindeer[5] " " reindeer[6] " " reindeer[7]
        }
        for (j = 1; j <= length(reindeers); j++) {
            split(reindeers[j], reindeer, " ")
            if (reindeer[4] == maxDistance) {
                reindeer[5]++
            }
            reindeers[j] = reindeer[1] " " reindeer[2] " " reindeer[3] " " reindeer[4] " " reindeer[5] " " reindeer[6] " " reindeer[7]
        }
    }
}

function findMaxPoints(reindeers) {
    maxPoints = 0
    for (i = 1; i <= length(reindeers); i++) {
        split(reindeers[i], reindeer, " ")
        if (reindeer[5] > maxPoints) {
            maxPoints = reindeer[5]
        }
    }
    return maxPoints
}
