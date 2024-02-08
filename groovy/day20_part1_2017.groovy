
def file = new File("input.txt")
def particles = file.readLines().collect { line ->
    def parts = line.split(", ")
    def p = [0, 0, 0]
    def v = [0, 0, 0]
    def a = [0, 0, 0]
    parts.eachWithIndex { part, i ->
        def coords = part[3..-2].split(",")
        coords.eachWithIndex { coord, j ->
            def num = coord as int
            switch (i) {
                case 0:
                    p[j] = num
                    break
                case 1:
                    v[j] = num
                    break
                case 2:
                    a[j] = num
                    break
            }
        }
    }
    [p: p, v: v, a: a]
}

def closestParticle = 0
def minAccel = Integer.MAX_VALUE
def minVelocity = Integer.MAX_VALUE
def minPosition = Integer.MAX_VALUE

particles.eachWithIndex { particle, i ->
    def accel = particle.a.sum { Math.abs(it) }
    def velocity = particle.v.sum { Math.abs(it) }
    def position = particle.p.sum { Math.abs(it) }

    if (accel < minAccel || (accel == minAccel && velocity < minVelocity) ||
            (accel == minAccel && velocity == minVelocity && position < minPosition)) {
        minAccel = accel
        minVelocity = velocity
        minPosition = position
        closestParticle = i
    }
}

println closestParticle
