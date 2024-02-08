
def abs(x) { x < 0 ? -x : x }

def max(a, b) { a > b ? a : b }

def distance(x, y, z) { (abs(x) + abs(y) + abs(z)) / 2 }

def file = new File("input.txt")
def input = file.text

def directions = input.split(",")

def x = 0, y = 0, z = 0
def maxDistance = 0

directions.each { dir ->
    switch (dir) {
        case "n":
            y++
            z--
            break
        case "ne":
            x++
            z--
            break
        case "se":
            x++
            y--
            break
        case "s":
            y--
            z++
            break
        case "sw":
            x--
            z++
            break
        case "nw":
            x--
            y++
            break
    }

    def curDistance = distance(x, y, z)
    maxDistance = max(maxDistance, curDistance)
}

println(distance(x, y, z))
