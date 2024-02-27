
{
    target = $1
    sideLength = int(sqrt(target) + 0.5)
    if (sideLength % 2 == 0) {
        sideLength++
    }

    maxValue = sideLength * sideLength
    stepsFromEdge = (sideLength - 1) / 2
    distanceToMiddle = 0

    for (i = 0; i < 4; i++) {
        middlePoint = maxValue - stepsFromEdge - (sideLength - 1) * i
        distance = target - middlePoint
        if (distance < 0) {
            distance *= -1
        }
        if (distance < distanceToMiddle || i == 0) {
            distanceToMiddle = distance
        }
    }

    manhattanDistance = stepsFromEdge + distanceToMiddle

    print manhattanDistance
}
