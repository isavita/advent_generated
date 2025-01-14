
def parseLine(String s) {
    def parts = s.trim().split(",")
    def xp = parts[0].trim()
    def yp = parts[1].trim()
    def x = parseVal(xp)
    def y = parseVal(yp)
    return [x, y]
}

def parsePrize(String s) {
    def parts = s.trim().split(",")
    def xp = parts[0].trim()
    def yp = parts[1].trim()
    def x = parseValPrize(xp)
    def y = parseValPrize(yp)
    return [x, y]
}

def parseVal(String s) {
    s = s.trim()
    s = s.replaceFirst("X\\+", "").replaceFirst("Y\\+", "").replaceFirst("X=", "").replaceFirst("Y=", "")
    return s.toInteger()
}

def parseValPrize(String s) {
    s = s.trim()
    s = s.replaceFirst("X=", "").replaceFirst("Y=", "")
    return s.toInteger()
}

def parseMachine(List<String> lines) {
    def m = [ax: 0, ay: 0, bx: 0, by: 0, px: 0, py: 0]
    lines.each { l ->
        l = l.replaceAll("Button A:", "A:").replaceAll("Button B:", "B:").replaceAll("Prize:", "P:")
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
    return m
}

def readInput(String filename) {
    def machines = []
    def lines = []
    new File(filename).eachLine { line ->
        line = line.trim()
        if (line.isEmpty()) {
            if (lines) {
                machines << parseMachine(lines)
                lines = []
            }
        } else {
            lines << line
        }
    }
    if (lines) {
        machines << parseMachine(lines)
    }
    return machines
}

def solveMachine(m) {
    def minCost = -1
    (0..100).each { aCount ->
        (0..100).each { bCount ->
            def x = m.ax * aCount + m.bx * bCount
            def y = m.ay * aCount + m.by * bCount
            if (x == m.px && y == m.py) {
                def cost = aCount * 3 + bCount
                if (minCost < 0 || cost < minCost) {
                    minCost = cost
                }
            }
        }
    }
    return minCost
}

def main() {
    def machines = readInput("input.txt")
    def results = machines.collect { m -> solveMachine(m) }.findAll { it >= 0 }

    if (results.isEmpty()) {
        println "0 0"
        return
    }

    def count = results.size()
    def sum = results.sum()
    println "$count $sum"
}

main()
