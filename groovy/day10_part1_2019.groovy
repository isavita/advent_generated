
def asteroids = new File("input.txt").readLines().collect { it.toList().collect { c -> c == '#' } as boolean[] }
def maxCount = findBestAsteroidLocation(asteroids)
println maxCount

int findBestAsteroidLocation(List<boolean[]> asteroids) {
    def maxCount = 0
    asteroids.eachWithIndex { row, y ->
        row.eachWithIndex { isAsteroid, x ->
            if (isAsteroid) {
                def count = countVisibleAsteroids(asteroids, x, y)
                if (count > maxCount) {
                    maxCount = count
                }
            }
        }
    }
    return maxCount
}

int countVisibleAsteroids(List<boolean[]> asteroids, int x, int y) {
    def angles = [:]
    asteroids.eachWithIndex { row, otherY ->
        row.eachWithIndex { isAsteroid, otherX ->
            if (isAsteroid && (otherX != x || otherY != y)) {
                def angle = Math.atan2(otherY - y, otherX - x)
                angles[angle] = true
            }
        }
    }
    return angles.size()
}
