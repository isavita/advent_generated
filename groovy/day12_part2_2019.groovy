
import java.util.regex.Matcher
import java.util.regex.Pattern

class Moon {
    int x, y, z, vx = 0, vy = 0, vz = 0

    Moon(int x, int y, int z) {
        this.x = x
        this.y = y
        this.z = z
    }

    int potentialEnergy() {
        Math.abs(x) + Math.abs(y) + Math.abs(z)
    }

    int kineticEnergy() {
        Math.abs(vx) + Math.abs(vy) + Math.abs(vz)
    }

    int totalEnergy() {
        potentialEnergy() * kineticEnergy()
    }

    @Override
    String toString() {
        "pos=<x=${x}, y=${y}, z=${z}>, vel=<x=${vx}, y=${vy}, z=${vz}>"
    }
}

static long gcd(long a, long b) {
    if (b == 0) {
        return a
    }
    return gcd(b, a % b)
}

static long lcm(long a, long b) {
    return (a * b) / gcd(a, b)
}

static void main(String[] args) {
    def input = new File('input.txt').readLines()

    Pattern pattern = ~/<x=(-?\d+), y=(-?\d+), z=(-?\d+)>/
    def moons = input.collect { String line ->
        Matcher matcher = pattern.matcher(line)
        if (matcher.find()) {
            int x = matcher.group(1).toInteger()
            int y = matcher.group(2).toInteger()
            int z = matcher.group(3).toInteger()
            new Moon(x, y, z)
        } else {
            null
        }
    }.findAll { it != null }


    // Part 1
    def moonsPart1 = moons.collect { new Moon(it.x, it.y, it.z) }
    int stepsPart1 = 1000

    for (int step = 0; step < stepsPart1; step++) {
        // Apply gravity
        for (int i = 0; i < moonsPart1.size(); i++) {
            for (int j = i + 1; j < moonsPart1.size(); j++) {
                def moon1 = moonsPart1[i]
                def moon2 = moonsPart1[j]

                if (moon1.x < moon2.x) {
                    moon1.vx++
                    moon2.vx--
                } else if (moon1.x > moon2.x) {
                    moon1.vx--
                    moon2.vx++
                }

                if (moon1.y < moon2.y) {
                    moon1.vy++
                    moon2.vy--
                } else if (moon1.y > moon2.y) {
                    moon1.vy--
                    moon2.vy++
                }

                if (moon1.z < moon2.z) {
                    moon1.vz++
                    moon2.vz--
                } else if (moon1.z > moon2.z) {
                    moon1.vz--
                    moon2.vz++
                }
            }
        }

        // Apply velocity
        moonsPart1.each { moon ->
            moon.x += moon.vx
            moon.y += moon.vy
            moon.z += moon.vz
        }

    }

    int totalEnergy = moonsPart1.sum { it.totalEnergy() }
    println "Part 1: Total energy after ${stepsPart1} steps: ${totalEnergy}"

    // Part 2
    def moonsPart2 = moons.collect { new Moon(it.x, it.y, it.z) }

    long periodX = 0
    long periodY = 0
    long periodZ = 0

    def initialXState = moonsPart2.collect { [it.x, it.vx] }
    def initialYState = moonsPart2.collect { [it.y, it.vy] }
    def initialZState = moonsPart2.collect { [it.z, it.vz] }

    long step = 0
    while (periodX == 0 || periodY == 0 || periodZ == 0) {
        // Apply gravity
        for (int i = 0; i < moonsPart2.size(); i++) {
            for (int j = i + 1; j < moonsPart2.size(); j++) {
                def moon1 = moonsPart2[i]
                def moon2 = moonsPart2[j]

                if (moon1.x < moon2.x) {
                    moon1.vx++
                    moon2.vx--
                } else if (moon1.x > moon2.x) {
                    moon1.vx--
                    moon2.vx++
                }

                if (moon1.y < moon2.y) {
                    moon1.vy++
                    moon2.vy--
                } else if (moon1.y > moon2.y) {
                    moon1.vy--
                    moon2.vy++
                }

                if (moon1.z < moon2.z) {
                    moon1.vz++
                    moon2.vz--
                } else if (moon1.z > moon2.z) {
                    moon1.vz--
                    moon2.vz++
                }
            }
        }

        // Apply velocity
        moonsPart2.each { moon ->
            moon.x += moon.vx
            moon.y += moon.vy
            moon.z += moon.vz
        }
        step++

        if (periodX == 0 && moonsPart2.collect { [it.x, it.vx] } == initialXState) {
            periodX = step
        }
        if (periodY == 0 && moonsPart2.collect { [it.y, it.vy] } == initialYState) {
            periodY = step
        }
        if (periodZ == 0 && moonsPart2.collect { [it.z, it.vz] } == initialZState) {
            periodZ = step
        }
    }

    long lcmXY = lcm(periodX, periodY)
    long lcmXYZ = lcm(lcmXY, periodZ)

    println "Part 2: Steps to repeat: ${lcmXYZ}"
}
