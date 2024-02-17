
function calculateNewFuel(currentPosition::Int, newPosition::Int)::Int
    diff = abs(currentPosition - newPosition)
    return (diff * (diff + 1)) ÷ 2
end

function abs(n::Int)::Int
    return n < 0 ? -n : n
end

open("input.txt") do file
    positions = parse.(Int, split(readline(file), ","))
    positions = sort(positions)

    min_fuel = typemax(Int) >> 1
    for i in positions[1]:positions[end]
        fuel = 0
        for pos in positions
            fuel += calculateNewFuel(pos, i)
        end
        min_fuel = min(min_fuel, fuel)
    end
    println(min_fuel)
end
