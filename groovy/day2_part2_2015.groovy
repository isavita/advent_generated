def totalPaper = 0
def totalRibbon = 0

new File('input.txt').eachLine { line ->
    def dimensions = line.split('x').collect { it.toInteger() }
    def l = dimensions[0]
    def w = dimensions[1]
    def h = dimensions[2]

    def side1 = l * w
    def side2 = w * h
    def side3 = h * l
    def slack = Math.min(side1, Math.min(side2, side3))

    totalPaper += 2*side1 + 2*side2 + 2*side3 + slack
    totalRibbon += 2*(l + w + h - Math.max(l, Math.max(w, h))) + l*w*h
}

println totalPaper
println totalRibbon