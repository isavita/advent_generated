using Printf

mutable struct Ship
    x::Int
    y::Int
    facing::Int
end

function main()
    file = open("input.txt")
    ship = Ship(0, 0, 0)
    
    for line in eachline(file)
        action = line[1]
        value = parse(Int, line[2:end])
        processInstruction(ship, action, value)
    end
    
    manhattanDistance = abs(ship.x) + abs(ship.y)
    println(manhattanDistance)
end

function processInstruction(ship::Ship, action::Char, value::Int)
    if action == 'N'
        ship.y += value
    elseif action == 'S'
        ship.y -= value
    elseif action == 'E'
        ship.x += value
    elseif action == 'W'
        ship.x -= value
    elseif action == 'L'
        ship.facing = (ship.facing - value + 360) % 360
    elseif action == 'R'
        ship.facing = (ship.facing + value) % 360
    elseif action == 'F'
        if ship.facing == 0
            ship.x += value
        elseif ship.facing == 90
            ship.y -= value
        elseif ship.facing == 180
            ship.x -= value
        elseif ship.facing == 270
            ship.y += value
        end
    end
end

function abs(x::Int)
    x < 0 ? -x : x
end

main()