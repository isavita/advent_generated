
def input = new File('input.txt').text.trim().split('\n')

def getCoords(path) {
    def x = 0, y = 0
    def coords = [:]
    def steps = 0

    path.each {
        def dir = it[0]
        def len = it[1..-1].toInteger()

        len.times {
            steps++
            switch (dir) {
                case 'U': y++; break
                case 'D': y--; break
                case 'L': x--; break
                case 'R': x++; break
            }
            coords["$x,$y"] = steps
        }
    }

    return coords
}

def wire1 = input[0].split(',').collect { it }
def wire2 = input[1].split(',').collect { it }

def coords1 = getCoords(wire1)
def coords2 = getCoords(wire2)

def intersections = coords1.keySet().intersect(coords2.keySet()) - '0,0'
def minDistance = intersections.collect { it.split(',').collect { it.toInteger() }.sum() }.min()

println minDistance
