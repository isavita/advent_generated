
using DelimitedFiles

function abs(x)
    if x < 0
        return -x
    end
    return x
end

function max(a, b)
    if a > b
        return a
    end
    return b
end

function distance(x, y, z)
    return (abs(x) + abs(y) + abs(z)) / 2
end

function main()
    input = readlines("input.txt")[1]
    directions = split(input, ",")

    x, y, z = 0, 0, 0
    max_distance = 0

    for dir in directions
        if dir == "n"
            y += 1
            z -= 1
        elseif dir == "ne"
            x += 1
            z -= 1
        elseif dir == "se"
            x += 1
            y -= 1
        elseif dir == "s"
            y -= 1
            z += 1
        elseif dir == "sw"
            x -= 1
            z += 1
        elseif dir == "nw"
            x -= 1
            y += 1
        end

        cur_distance = distance(x, y, z)
        max_distance = max(max_distance, cur_distance)
    end

    println(distance(x, y, z))
end

main()
