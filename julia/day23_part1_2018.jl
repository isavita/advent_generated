
struct Nanobot
    x::Int
    y::Int
    z::Int
    radius::Int
end

function parseNanobots(file)
    nanobots = []
    re = r"pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)"
    
    for line in eachline(file)
        matches = match(re, line)
        x = parse(Int, matches[1])
        y = parse(Int, matches[2])
        z = parse(Int, matches[3])
        radius = parse(Int, matches[4])
        
        push!(nanobots, Nanobot(x, y, z, radius))
    end
    
    return nanobots
end

function findStrongestNanobot(nanobots)
    strongest = Nanobot(0, 0, 0, 0)
    for nanobot in nanobots
        if nanobot.radius > strongest.radius
            strongest = nanobot
        end
    end
    return strongest
end

function countNanobotsInRange(nanobots, strongest)
    count = 0
    for nanobot in nanobots
        if manhattanDistance(nanobot, strongest) <= strongest.radius
            count += 1
        end
    end
    return count
end

function manhattanDistance(a, b)
    return abs(a.x - b.x) + abs(a.y - b.y) + abs(a.z - b.z)
end

function abs(x)
    return x < 0 ? -x : x
end

file = open("input.txt")
nanobots = parseNanobots(file)
strongest = findStrongestNanobot(nanobots)
inRangeCount = countNanobotsInRange(nanobots, strongest)
println(inRangeCount)
close(file)
