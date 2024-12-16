
def solve() {
    def machines = readInput("input.txt")
    machines = machines.collect { m ->
        [ax: m.ax, ay: m.ay, bx: m.bx, by: m.by, px: m.px + 10000000000000, py: m.py + 10000000000000]
    }
    def results = machines.collect { m -> solveMachine(m) }.findAll { it >= 0 }
    if (results.isEmpty()) {
        println "0 0"
    } else {
        println "${results.size()} ${results.sum()}"
    }
}

def readInput(filename) {
    def machines = []
    def lines = []
    new File(filename).eachLine { line ->
        line = line.trim()
        if (line.isEmpty()) {
            if (!lines.isEmpty()) {
                machines << parseMachine(lines)
                lines = []
            }
        } else {
            lines << line
        }
    }
    if (!lines.isEmpty()) {
        machines << parseMachine(lines)
    }
    machines
}

def parseMachine(lines) {
    def m = [ax: 0, ay: 0, bx: 0, by: 0, px: 0, py: 0]
    lines.each { l ->
        l = l.replace("Button A:", "A:").replace("Button B:", "B:").replace("Prize:", "P:")
        if (l.startsWith("A:")) {
            def (x, y) = parseLine(l[2..-1])
            m.ax = x
            m.ay = y
        } else if (l.startsWith("B:")) {
            def (x, y) = parseLine(l[2..-1])
            m.bx = x
            m.by = y
        } else if (l.startsWith("P:")) {
            def (x, y) = parsePrize(l[2..-1])
            m.px = x
            m.py = y
        }
    }
    m
}

def parseLine(s) {
    def parts = s.trim().split(",")
    def x = parseVal(parts[0])
    def y = parseVal(parts[1])
    return [x, y]
}

def parsePrize(s) {
    def parts = s.trim().split(",")
    def x = parseValPrize(parts[0])
    def y = parseValPrize(parts[1])
    return [x, y]
}

def parseVal(s) {
    s = s.trim().replaceFirst("X[+=]?|Y[+=]?", "")
    return s.toLong()
}

def parseValPrize(s) {
    s = s.trim().replaceFirst("X=|Y=", "")
    return s.toLong()
}

def solveMachine(m) {
    def D = m.ax * m.by - m.ay * m.bx
    if (D == 0) return -1
    def numA = m.px * m.by - m.py * m.bx
    def numB = -m.px * m.ay + m.py * m.ax
    if (numA % D != 0 || numB % D != 0) return -1
    def a = numA / D
    def b = numB / D
    if (a < 0 || b < 0) return -1
    return 3 * a + b
}

solve()
