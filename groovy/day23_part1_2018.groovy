
def file = new File("input.txt")
def nanobots = file.readLines().collect { line ->
    def matcher = line =~ /pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)/
    def (x, y, z, radius) = matcher[0][1..4].collect { it as int }
    [x: x, y: y, z: z, radius: radius]
}

def strongest = nanobots.max { it.radius }
def inRangeCount = nanobots.count { manhattanDistance(it, strongest) <= strongest.radius }

println inRangeCount

def manhattanDistance(a, b) {
    return Math.abs(a.x - b.x) + Math.abs(a.y - b.y) + Math.abs(a.z - b.z)
}
