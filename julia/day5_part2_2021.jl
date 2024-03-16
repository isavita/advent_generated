function abs(x::Int)
    return x < 0 ? -x : x
end

function sign(x::Int)
    if x > 0
        return 1
    elseif x < 0
        return -1
    else
        return 0
    end
end

lines = []
open("input.txt", "r") do file
    for line in eachline(file)
        parts = split(line, " -> ")
        start = split(parts[1], ",")
        endd = split(parts[2], ",")
        x1 = parse(Int, start[1])
        y1 = parse(Int, start[2])
        x2 = parse(Int, endd[1])
        y2 = parse(Int, endd[2])
        push!(lines, [x1, y1, x2, y2])
    end
end

overlaps = Dict{Tuple{Int,Int},Int}()

for line in lines
    x1, y1, x2, y2 = line
    xStep = sign(x2 - x1)
    yStep = sign(y2 - y1)
    steps = max(abs(x2 - x1) + 1, abs(y2 - y1) + 1)
    for i in 0:(steps-1)
        point = (x1 + i*xStep, y1 + i*yStep)
        overlaps[point] = get(overlaps, point, 0) + 1
    end
end

global count = 0
for v in values(overlaps)
    if v > 1
        global count += 1
    end
end

println(count)