
def input = new File("input.txt").readLines()

def x = 0
def y = 0
def wx = 10
def wy = 1

input.each {
    def action = it[0]
    def value = it[1..-1] as int

    switch(action) {
        case "N":
            wy += value
            break
        case "S":
            wy -= value
            break
        case "E":
            wx += value
            break
        case "W":
            wx -= value
            break
        case "L":
            def times = value / 90
            times.times {
                def temp = wx
                wx = -wy
                wy = temp
            }
            break
        case "R":
            def times = value / 90
            times.times {
                def temp = wx
                wx = wy
                wy = -temp
            }
            break
        case "F":
            x += wx * value
            y += wy * value
            break
    }
}

println Math.abs(x) + Math.abs(y)