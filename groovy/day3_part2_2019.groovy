
def data = new File("input.txt").text.trim().split("\n")
def wire1 = getPointsWithSteps(data[0])
def wire2 = getPointsWithSteps(data[1])

def minSteps = Integer.MAX_VALUE
wire1.each { p, steps1 ->
    def steps2 = wire2[p]
    if (steps2 != null) {
        def totalSteps = steps1 + steps2
        if (totalSteps < minSteps) {
            minSteps = totalSteps
        }
    }
}

println minSteps

def getPointsWithSteps(path) {
    def points = [:]
    def current = [X: 0, Y: 0]
    def steps = 0
    path.split(',').each { move ->
        def dir = move.charAt(0)
        def dist = move.substring(1) as Integer
        (1..dist).each {
            steps++
            switch (dir) {
                case 'U':
                    current.Y++
                    break
                case 'D':
                    current.Y--
                    break
                case 'L':
                    current.X--
                    break
                case 'R':
                    current.X++
                    break
            }
            if (!points.containsKey([X: current.X, Y: current.Y])) {
                points[[X: current.X, Y: current.Y]] = steps
            }
        }
    }
    return points
}
