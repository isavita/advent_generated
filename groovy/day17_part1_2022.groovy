
def getRockShapes() {
    [
        [[0, 0], [1, 0], [2, 0], [3, 0]],
        [[1, 0], [0, 1], [1, 1], [2, 1], [1, 2]],
        [[0, 0], [1, 0], [2, 0], [2, 1], [2, 2]],
        [[0, 0], [0, 1], [0, 2], [0, 3]],
        [[0, 0], [1, 0], [0, 1], [1, 1]]
    ]
}

def canMove(rock, direction, chamber, highestY) {
    def movedRock = []
    for (coords in rock) {
        def x = coords[0]
        def y = coords[1]
        def newX, newY
        switch (direction) {
            case 'left':
                newX = x - 1; newY = y; break
            case 'right':
                newX = x + 1; newY = y; break
            case 'down':
                newX = x; newY = y - 1; break
            default:
                throw new IllegalArgumentException("Invalid direction")
        }

        if (newX < 0 || newX > 6 || newY < 1 || chamber.contains([newX, newY])) {
            return false
        }
        movedRock.add([newX, newY])
    }
    movedRock
}

def simulate(jetPattern, totalRocks) {
    def rockShapes = getRockShapes()
    def chamber = (0..6).collect { [it, 0] } as Set
    def highestY = 0
    def jetLen = jetPattern.length()
    def jetIndex = 0

    (0..<totalRocks).each { rockNumber ->
        def shape = rockShapes[rockNumber % rockShapes.size()]
        def rockX = 2
        def rockY = highestY + 4
        def rock = shape.collect { [rockX + it[0], rockY + it[1]] }

        while (true) {
            def jetDir = jetPattern[jetIndex % jetLen]
            jetIndex++

            def movedRock = canMove(rock, jetDir == '>' ? 'right' : 'left', chamber, highestY)
            if (movedRock) {
                rock = movedRock
            }

            def movedDown = canMove(rock, 'down', chamber, highestY)
            if (movedDown) {
                rock = movedDown
            } else {
                rock.each {
                    chamber.add(it)
                    highestY = Math.max(highestY, it[1])
                }
                break
            }
        }
    }
    highestY
}

def main() {
    def jetPattern = new File('input.txt').text.trim()
    def totalRocks = 2022
    println simulate(jetPattern, totalRocks)
}

main()
