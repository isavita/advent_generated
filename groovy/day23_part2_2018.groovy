
import java.util.regex.Pattern

def parseInput(String filePath) {
    def nanobots = []
    def pattern = Pattern.compile(/pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)/)
    new File(filePath).eachLine { line ->
        def matcher = pattern.matcher(line.strip())
        if (matcher.matches()) {
            def x = matcher.group(1).toInteger()
            def y = matcher.group(2).toInteger()
            def z = matcher.group(3).toInteger()
            def r = matcher.group(4).toInteger()
            nanobots << [x, y, z, r]
        }
    }
    nanobots
}

def manhattanDistance(a, b) {
    Math.abs(a[0] - b[0]) + Math.abs(a[1] - b[1]) + Math.abs(a[2] - b[2])
}

def partOne(nanobots) {
    def strongest = nanobots.max { it[3] }
    def (sx, sy, sz, sr) = strongest
    nanobots.count {
        manhattanDistance([sx, sy, sz], [it[0], it[1], it[2]]) <= sr
    }
}

def minDistanceToOrigin(x, y, z, size) {
    def dx = x > 0 ? x : (x + size - 1 < 0 ? -(x + size - 1) : 0)
    def dy = y > 0 ? y : (y + size - 1 < 0 ? -(y + size - 1) : 0)
    def dz = z > 0 ? z : (z + size - 1 < 0 ? -(z + size - 1) : 0)
    dx + dy + dz
}

def partTwo(nanobots) {
    def minX = nanobots.min { it[0] }[0]
    def maxX = nanobots.max { it[0] }[0]
    def minY = nanobots.min { it[1] }[1]
    def maxY = nanobots.max { it[1] }[1]
    def minZ = nanobots.min { it[2] }[2]
    def maxZ = nanobots.max { it[2] }[2]

    def size = 1
    while (size < [maxX - minX, maxY - minY, maxZ - minZ].max()) {
        size *= 2
    }

    def heap = [[0, minDistanceToOrigin(minX, minY, minZ, size), size, minX, minY, minZ]]

    def bestDistance = null
    def bestCount = -1

    while (heap) {
        def (negCount, distance, currentSize, x, y, z) = heap.min { a, b ->
            a[0] <=> b[0] ?: a[1] <=> b[1] ?: a[2] <=> b[2]
        }
        heap.remove(heap.min { a, b ->
            a[0] <=> b[0] ?: a[1] <=> b[1] ?: a[2] <=> b[2]
        })

        def count = -negCount

        if (currentSize == 1) {
            if (count > bestCount || (count == bestCount && distance < bestDistance)) {
                bestCount = count
                bestDistance = distance
                break
            }
            continue
        }

        def half = currentSize / 2
        [0, half].each { dx ->
            [0, half].each { dy ->
                [0, half].each { dz ->
                    def nx = x + dx
                    def ny = y + dy
                    def nz = z + dz
                    def newSize = half > 0 ? half: 1
                    def botInRangeCount = 0
                    nanobots.each { bot ->
                        def (bx, by, bz, br) = bot
                        def d = 0
                        d += bx < nx ? nx - bx : (bx > nx + newSize - 1 ? bx - (nx + newSize - 1) : 0)
                        d += by < ny ? ny - by : (by > ny + newSize - 1 ? by - (ny + newSize - 1) : 0)
                        d += bz < nz ? nz - bz : (bz > nz + newSize - 1 ? bz - (nz + newSize - 1) : 0)

                        if (d <= br) {
                            botInRangeCount++
                        }
                    }
                    def newDistance = minDistanceToOrigin(nx, ny, nz, newSize)
                    heap << [-botInRangeCount, newDistance, newSize, nx, ny, nz]
                }
            }
        }
    }
    bestDistance
}

static void main(args) {
    def nanobots = parseInput('input.txt')

    def partOneResult = partOne(nanobots)
    println(partOneResult)

    def partTwoResult = partTwo(nanobots)
    println(partTwoResult)
}
