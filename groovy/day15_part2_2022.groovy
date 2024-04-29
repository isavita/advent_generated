def sensors = []
new File('input.txt').eachLine { line ->
    def match = line =~ /Sensor at x=([-0-9]+), y=([-0-9]+): closest beacon is at x=([-0-9]+), y=([-0-9]+)/
    if (match) {
        def sx = match[0][1] as int
        def sy = match[0][2] as int
        def bx = match[0][3] as int
        def by = match[0][4] as int
        sensors.add([sx, sy, bx, by])
    }
}

def row = 2000000
def cannotBe = [] as Set
sensors.each { sx, sy, bx, by ->
    def distance = Math.abs(sx - bx) + Math.abs(sy - by)
    def yDistance = Math.abs(sy - row)
    if (yDistance <= distance) {
        def xDistance = distance - yDistance
        for (int x = sx - xDistance; x <= sx + xDistance; x++) {
            cannotBe.add(x)
        }
    }
}

println "Part 1: ${cannotBe.size()}"

def minX = 0
def maxX = 4000000
def minY = 0
def maxY = 4000000

def found = false
for (int y = minY; y <= maxY && !found; y++) {
    def intervals = []
    sensors.each { sx, sy, bx, by ->
        def distance = Math.abs(sx - bx) + Math.abs(sy - by)
        def yDistance = Math.abs(sy - y)
        if (yDistance <= distance) {
            def xDistance = distance - yDistance
            intervals.add([sx - xDistance, sx + xDistance])
        }
    }
    intervals.sort { it[0] }
    def x = minX
    for (interval in intervals) {
        if (interval[0] > x) {
            println "Part 2: ${x * 4000000L + y}"
            found = true
            break
        } else if (interval[1] > x) {
            x = interval[1] + 1
        }
    }
}