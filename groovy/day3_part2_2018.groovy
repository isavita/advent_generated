
def claims = new File("input.txt").readLines().collect {
    def parts = it.split(" ")
    def id = parts[0][1..-1] as int
    def coords = parts[2][0..-2].split(",")
    def x = coords[0] as int
    def y = coords[1] as int
    def dims = parts[3].split("x")
    def width = dims[0] as int
    def height = dims[1] as int
    [id: id, x: x, y: y, width: width, height: height]
}

def fabric = new int[1000][1000]

claims.each { claim ->
    (claim.y..<claim.y + claim.height).each { y ->
        (claim.x..<claim.x + claim.width).each { x ->
            fabric[y][x]++
        }
    }
}

claims.find { claim ->
    def overlap = false
    (claim.y..<claim.y + claim.height).each { y ->
        (claim.x..<claim.x + claim.width).each { x ->
            if (fabric[y][x] > 1) {
                overlap = true
                return
            }
        }
        if (overlap) {
            return
        }
    }
    if (!overlap) {
        println claim.id
        return
    }
}
