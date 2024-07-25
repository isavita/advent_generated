
class Moon {
    int x, y, z
    int vx = 0, vy = 0, vz = 0

    Moon(int x, int y, int z) {
        this.x = x
        this.y = y
        this.z = z
    }

    void applyGravity(Moon other) {
        if (x < other.x) vx++
        else if (x > other.x) vx--

        if (y < other.y) vy++
        else if (y > other.y) vy--

        if (z < other.z) vz++
        else if (z > other.z) vz--
    }

    void updatePosition() {
        x += vx
        y += vy
        z += vz
    }

    int potentialEnergy() {
        return Math.abs(x) + Math.abs(y) + Math.abs(z)
    }

    int kineticEnergy() {
        return Math.abs(vx) + Math.abs(vy) + Math.abs(vz)
    }

    int totalEnergy() {
        return potentialEnergy() * kineticEnergy()
    }
}

def readMoons(String filename) {
    def moons = []
    new File(filename).eachLine { line ->
        def matcher = line =~ /<x=(-?\d+), y=(-?\d+), z=(-?\d+)>/
        if (matcher) {
            moons << new Moon(matcher[0][1] as int, matcher[0][2] as int, matcher[0][3] as int)
        }
    }
    return moons
}

def simulateMoons(moons, steps) {
    for (int step = 0; step < steps; step++) {
        // Apply gravity
        for (int i = 0; i < moons.size(); i++) {
            for (int j = i + 1; j < moons.size(); j++) {
                moons[i].applyGravity(moons[j])
                moons[j].applyGravity(moons[i])
            }
        }
        // Update positions
        moons.each { it.updatePosition() }
    }
}

def calculateTotalEnergy(moons) {
    return moons.sum { it.totalEnergy() }
}

def main() {
    def moons = readMoons("input.txt")
    simulateMoons(moons, 1000)
    def totalEnergy = calculateTotalEnergy(moons)
    println "Total energy after 1000 steps: $totalEnergy"
}

main()
