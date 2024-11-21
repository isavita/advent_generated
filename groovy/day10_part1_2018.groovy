
def input = new File('input.txt').readLines()
def stars = input.collect { line ->
    def (x, y, vx, vy) = line.replaceAll(/position=<\s*(-?\d+),\s*(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+)>/, '$1 $2 $3 $4').split().collect { it.toInteger() }
    [x: x, y: y, vx: vx, vy: vy]
}

def smallestT = 0
def smallestArea = Integer.MAX_VALUE

for (t in 1..<100000) {
    def (minX, minY, maxX, maxY) = [Integer.MAX_VALUE, Integer.MAX_VALUE, Integer.MIN_VALUE, Integer.MIN_VALUE]
    stars.each { star ->
        def x = star.x + star.vx * t
        def y = star.y + star.vy * t
        minX = Math.min(minX, x)
        minY = Math.min(minY, y)
        maxX = Math.max(maxX, x)
        maxY = Math.max(maxY, y)
    }
    def area = (maxX - minX + 1) + (maxY - minY + 1)
    if (area < smallestArea) {
        smallestArea = area
        smallestT = t
    }
}

def t = smallestT
def (minX, minY, maxX, maxY) = [Integer.MAX_VALUE, Integer.MAX_VALUE, Integer.MIN_VALUE, Integer.MIN_VALUE]
stars.each { star ->
    star.x += star.vx * t
    star.y += star.vy * t
    minX = Math.min(minX, star.x)
    minY = Math.min(minY, star.y)
    maxX = Math.max(maxX, star.x)
    maxY = Math.max(maxY, star.y)
}

def mapper = (0..(maxY - minY)).collect { (0..(maxX - minX)).collect { false } }
stars.each { star -> mapper[star.y - minY][star.x - minX] = true }

mapper.each { row ->
    row.each { cell -> print(cell ? '#' : ' ') }
    println()
}
