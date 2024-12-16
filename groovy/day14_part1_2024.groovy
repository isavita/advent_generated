
def width = 101
def height = 103
def robots = new File("input.txt").readLines().collect { line ->
    def parts = line.split(" ")
    def pos = parts[0].substring(2).split(",").collect { it.toInteger() }
    def vel = parts[1].substring(2).split(",").collect { it.toInteger() }
    [pos[0], pos[1], vel[0], vel[1]]
}

100.times {
    robots.each { r ->
        r[0] = (r[0] + r[2]) % width
        r[1] = (r[1] + r[3]) % height
        if (r[0] < 0) r[0] += width
        if (r[1] < 0) r[1] += height
    }
}

def q1 = 0
def q2 = 0
def q3 = 0
def q4 = 0

robots.each { r ->
    if (r[0] == 50 || r[1] == 51) return
    if (r[0] < 50 && r[1] < 51) q1++
    if (r[0] > 50 && r[1] < 51) q2++
    if (r[0] < 50 && r[1] > 51) q3++
    if (r[0] > 50 && r[1] > 51) q4++
}

println q1 * q2 * q3 * q4
