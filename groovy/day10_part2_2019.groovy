
def asteroids = new File("input.txt").readLines().collect { it.toList().collect { c -> c == '#' } }

def vaporizeAsteroids(asteroids, station) {
    def targets = []
    asteroids.eachWithIndex { row, y ->
        row.eachWithIndex { isAsteroid, x ->
            if (isAsteroid && !(x == station[0] && y == station[1])) {
                def angle = Math.atan2(y - station[1], x - station[0])
                def dist = Math.hypot(x - station[0], y - station[1])
                if (angle < -Math.PI/2) {
                    angle += 2 * Math.PI
                }
                targets << [x: x, y: y, angle: angle, dist: dist]
            }
        }
    }

    targets = targets.sort { a, b ->
        if (a.angle == b.angle) {
            a.dist <=> b.dist
        } else {
            a.angle <=> b.angle
        }
    }

    def vaporized = []
    while (targets) {
        def lastAngle = -Double.MAX_VALUE
        targets.removeIf { target ->
            if (target.angle != lastAngle) {
                vaporized << target
                lastAngle = target.angle
                true
            } else {
                false
            }
        }
    }

    return vaporized
}

def findBestAsteroidLocation(asteroids) {
    def maxCount = 0
    def bestLocation = [0, 0]
    asteroids.eachWithIndex { row, y ->
        row.eachWithIndex { isAsteroid, x ->
            if (isAsteroid) {
                def count = countVisibleAsteroids(asteroids, x, y)
                if (count > maxCount) {
                    maxCount = count
                    bestLocation = [x, y]
                }
            }
        }
    }
    return [bestLocation, maxCount]
}

def countVisibleAsteroids(asteroids, x, y) {
    def angles = [:].withDefault { false }
    asteroids.eachWithIndex { row, otherY ->
        row.eachWithIndex { isAsteroid, otherX ->
            if (isAsteroid && !(otherX == x && otherY == y)) {
                def angle = Math.atan2(otherY - y, otherX - x)
                angles[angle] = true
            }
        }
    }
    return angles.size()
}

def (bestLocation, _) = findBestAsteroidLocation(asteroids)
def vaporized = vaporizeAsteroids(asteroids, bestLocation)
if (vaporized.size() >= 200) {
    def result = vaporized[199].x * 100 + vaporized[199].y
    println(result)
} else {
    println("Less than 200 asteroids were vaporized.")
}
