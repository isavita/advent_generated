def totalPaper = 0

new File("input.txt").eachLine { line ->
    def dimensions = line.split("x").collect { it.toInteger() }
    def l = dimensions[0]
    def w = dimensions[1]
    def h = dimensions[2]

    def side1 = l * w
    def side2 = w * h
    def side3 = h * l

    def extra = Math.min(side1, Math.min(side2, side3))

    totalPaper += 2*side1 + 2*side2 + 2*side3 + extra
}

println totalPaper