
def mod(a, b) { (a % b + b) % b }

def parseLine(line) {
    def matcher = line =~ /p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)/
    if (!matcher.find()) throw new Exception("invalid line format: $line")
    [x: matcher[0][1].toInteger(), y: matcher[0][2].toInteger(), vx: matcher[0][3].toInteger(), vy: matcher[0][4].toInteger()]
}

def moveRobots(robots, sizeX, sizeY) {
    robots.each { it.x = mod(it.x + it.vx, sizeX); it.y = mod(it.y + it.vy, sizeY) }
}

def countQuadrants(robots, sizeX, sizeY) {
    def centerX = sizeX / 2
    def centerY = sizeY / 2
    def counts = [0, 0, 0, 0]
    robots.each {
        if (it.x < centerX) {
            if (it.y < centerY) counts[0]++
            else if (it.y > centerY) counts[1]++
        } else if (it.x > centerX) {
            if (it.y < centerY) counts[2]++
            else if (it.y > centerY) counts[3]++
        }
    }
    counts
}

def hasNoOverlaps(robots) {
    def positions = robots.collect { [it.x, it.y] }
    positions.size() == positions.unique().size()
}

def drawGrid(robots, sizeX, sizeY) {
    def grid = (0..<sizeY).collect { (0..<sizeX).collect { '.' } }
    robots.each { grid[it.y][it.x] = '#' }
    grid.each { println it.join() }
}

def solve() {
    def sizeX = 101
    def sizeY = 103
    def robots = new File("input.txt").readLines().findAll { it }.collect { parseLine(it) }

    def robotsPart1 = robots.collect { it.clone() }
    100.times { moveRobots(robotsPart1, sizeX, sizeY) }
    def counts = countQuadrants(robotsPart1, sizeX, sizeY)
    def safetyFactor = counts.inject(1) { acc, c -> acc * c }
    println "Part 1 - Safety Factor after 100 seconds: $safetyFactor"

    def robotsPart2 = robots.collect { it.clone() }
    def seconds = 0
    while (!hasNoOverlaps(robotsPart2)) {
        moveRobots(robotsPart2, sizeX, sizeY)
        seconds++
        if (seconds > 1000000) {
            println "Exceeded maximum iterations without finding a unique position configuration."
            return
        }
    }
    println "Part 2 - Fewest seconds to display Easter egg: $seconds"
    println "Final positions of robots:"
    drawGrid(robotsPart2, sizeX, sizeY)
}

solve()
