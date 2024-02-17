
using DelimitedFiles

function abs(x::Int)::Int
    if x < 0
        return -x
    end
    return x
end

function max(a::Int, b::Int)::Int
    if a > b
        return a
    end
    return b
end

function distance(x::Int, y::Int, z::Int)::Int
    return (abs(x) + abs(y) + abs(z)) ÷ 2
end

input = readdlm("input.txt")[1]
directions = split(input, ",")

x, y, z = 0, 0, 0
max_distance = 0

for dir in directions
    if dir == "n"
        global y += 1
        global z -= 1
    elseif dir == "ne"
        global x += 1
        global z -= 1
    elseif dir == "se"
        global x += 1
        global y -= 1
    elseif dir == "s"
        global y -= 1
        global z += 1
    elseif dir == "sw"
        global x -= 1
        global z += 1
    elseif dir == "nw"
        global x -= 1
        global y += 1
    end

    cur_distance = distance(x, y, z)
    global max_distance = max(max_distance, cur_distance)
end

println(max_distance)
