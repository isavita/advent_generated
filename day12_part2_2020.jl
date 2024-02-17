
struct Ship
    x::Int
    y::Int
    waypointX::Int
    waypointY::Int
end

function processInstruction(ship::Ship, action::Char, value::Int)
    if action == 'N'
        ship = Ship(ship.x, ship.y, ship.waypointX, ship.waypointY + value)
    elseif action == 'S'
        ship = Ship(ship.x, ship.y, ship.waypointX, ship.waypointY - value)
    elseif action == 'E'
        ship = Ship(ship.x, ship.y, ship.waypointX + value, ship.waypointY)
    elseif action == 'W'
        ship = Ship(ship.x, ship.y, ship.waypointX - value, ship.waypointY)
    elseif action == 'L'
        ship = rotateWaypoint(ship, -value)
    elseif action == 'R'
        ship = rotateWaypoint(ship, value)
    elseif action == 'F'
        ship = Ship(ship.x + ship.waypointX * value, ship.y + ship.waypointY * value, ship.waypointX, ship.waypointY)
    end
    return ship
end

function rotateWaypoint(ship::Ship, degrees::Int)
    degrees = (degrees + 360) % 360
    if degrees == 90 || degrees == -270
        ship = Ship(ship.x, ship.y, ship.waypointY, -ship.waypointX)
    elseif degrees == 180 || degrees == -180
        ship = Ship(ship.x, ship.y, -ship.waypointX, -ship.waypointY)
    elseif degrees == 270 || degrees == -90
        ship = Ship(ship.x, ship.y, -ship.waypointY, ship.waypointX)
    end
    return ship
end

function abs(x::Int)
    if x < 0
        return -x
    end
    return x
end

function main()
    file = open("input.txt")
    ship = Ship(0, 0, 10, 1)
    
    for line in eachline(file)
        action = line[1]
        value = parse(Int, line[2:end])
        ship = processInstruction(ship, action, value)
    end
    
    manhattanDistance = abs(ship.x) + abs(ship.y)
    println(manhattanDistance)
end

main()
