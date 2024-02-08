
def serialNumber = new File('input.txt').text.toInteger()
def grid = (1..300).collect { x ->
    (1..300).collect { y ->
        def rackId = x + 10
        def powerLevel = (((rackId * y) + serialNumber) * rackId).toString()[-3] as Integer - 5
        [x, y, powerLevel]
    }
}

def maxPower = 0
def coordinates = [0, 0]

(1..298).each { x ->
    (1..298).each { y ->
        def squarePower = (x..x+2).sum { i ->
            (y..y+2).sum { j ->
                grid[i-1][j-1][2]
            }
        }
        if (squarePower > maxPower) {
            maxPower = squarePower
            coordinates = [x, y]
        }
    }
}

println "${coordinates[0]},${coordinates[1]}"
