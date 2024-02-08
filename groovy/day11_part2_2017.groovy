def input = new File("input.txt").text.tokenize(",")
def x = 0, y = 0, z = 0, maxDistance = 0

input.each {
    if (it == "n") { y++; z-- }
    else if (it == "ne") { x++; z-- }
    else if (it == "se") { x++; y-- }
    else if (it == "s") { y--; z++ }
    else if (it == "sw") { x--; z++ }
    else if (it == "nw") { x--; y++ }

    def distance = (Math.abs(x) + Math.abs(y) + Math.abs(z)) / 2
    if (distance > maxDistance) {
        maxDistance = distance
    }
}

println(maxDistance)