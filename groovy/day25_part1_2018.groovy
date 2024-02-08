def input = new File("input.txt").readLines()
def points = input.collect { it.split(",").collect { it.toInteger() } }

def findClosePoints(p1, p2) {
    def dist = 0
    for (int i = 0; i < p1.size(); i++) {
        dist += Math.abs(p1[i] - p2[i])
    }
    return dist <= 3
}

def constellations = 0
def visited = new HashSet()

for (int i = 0; i < points.size(); i++) {
    if (visited.contains(i)) continue
    visited.add(i)
    def queue = [i]

    while (!queue.isEmpty()) {
        def current = queue.pop()

        for (int j = 0; j < points.size(); j++) {
            if (visited.contains(j)) continue
            if (findClosePoints(points[current], points[j])) {
                visited.add(j)
                queue.add(j)
            }
        }
    }

    constellations++
}

println constellations