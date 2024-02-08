
def file = new File("input.txt")
def lines = []
file.eachLine { line ->
    def parts = line.split(" -> ")
    def start = parts[0].split(",")
    def end = parts[1].split(",")

    def x1 = start[0] as int
    def y1 = start[1] as int
    def x2 = end[0] as int
    def y2 = end[1] as int

    lines.add([x1, y1, x2, y2] as int[])
}

def overlaps = [:]

lines.each { line ->
    def x1 = line[0]
    def y1 = line[1]
    def x2 = line[2]
    def y2 = line[3]

    def xStep = (x2 - x1) <=> 0
    def yStep = (y2 - y1) <=> 0
    def steps = Math.abs(x2 - x1) + 1
    if (Math.abs(y2 - y1) > Math.abs(x2 - x1)) {
        steps = Math.abs(y2 - y1) + 1
    }

    (0..<steps).each { i ->
        def point = [x1 + i*xStep, y1 + i*yStep]
        if (!overlaps.containsKey(point)) {
            overlaps[point] = 1
        } else {
            overlaps[point]++
        }
    }
}

def count = overlaps.findAll { it.value > 1 }.size()
println(count)
