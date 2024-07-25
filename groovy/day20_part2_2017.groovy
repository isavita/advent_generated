
import java.nio.file.Files
import java.nio.file.Paths

class Particle {
    int id
    int[] position
    int[] velocity
    int[] acceleration

    Particle(int id, String line) {
        this.id = id
        def matcher = (line =~ /p=<([^>]+)>, v=<([^>]+)>, a=<([^>]+)>/)
        if (matcher) {
            this.position = matcher[0][1].split(',').collect { it as int }
            this.velocity = matcher[0][2].split(',').collect { it as int }
            this.acceleration = matcher[0][3].split(',').collect { it as int }
        }
    }

    void update() {
        // Update velocity with acceleration
        for (int i = 0; i < 3; i++) {
            velocity[i] += acceleration[i]
        }
        // Update position with velocity
        for (int i = 0; i < 3; i++) {
            position[i] += velocity[i]
        }
    }

    int manhattanDistance() {
        return position.collect { Math.abs(it) }.sum()
    }

    String toString() {
        return "Particle $id: p=${position.join(',')}, v=${velocity.join(',')}, a=${acceleration.join(',')}"
    }
}

def readParticles(String filename) {
    def particles = []
    Files.lines(Paths.get(filename)).eachWithIndex { line, index ->
        particles << new Particle(index, line)
    }
    return particles
}

def simulateParticles(List<Particle> particles, int ticks) {
    for (int i = 0; i < ticks; i++) {
        def positions = [:]
        particles.each { particle ->
            particle.update()
            def posKey = particle.position.join(',')
            positions[posKey] = (positions[posKey] ?: []) + particle
        }
        // Remove collided particles
        particles.removeAll { particle ->
            positions[particle.position.join(',')].size() > 1
        }
    }
    return particles.size()
}

def main() {
    def particles = readParticles("input.txt")
    int remainingParticles = simulateParticles(particles, 1000)
    println "Particles remaining after collisions: $remainingParticles"
}

main()
