
def file = new File("input.txt")
def ship = [x: 0, y: 0, facing: 0]

file.eachLine { line ->
    def action = line[0]
    def value = line.substring(1) as Integer
    processInstruction(ship, action, value)
}

def manhattanDistance = Math.abs(ship.x) + Math.abs(ship.y)
println(manhattanDistance)

def processInstruction(ship, action, value) {
    switch (action) {
        case 'N':
            ship.y += value
            break
        case 'S':
            ship.y -= value
            break
        case 'E':
            ship.x += value
            break
        case 'W':
            ship.x -= value
            break
        case 'L':
            ship.facing = (ship.facing - value + 360) % 360
            break
        case 'R':
            ship.facing = (ship.facing + value) % 360
            break
        case 'F':
            switch (ship.facing) {
                case 0:
                    ship.x += value
                    break
                case 90:
                    ship.y -= value
                    break
                case 180:
                    ship.x -= value
                    break
                case 270:
                    ship.y += value
                    break
            }
            break
    }
}
