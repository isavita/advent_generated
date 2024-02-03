
function parseNanobots(file)
    nanobots = {}
    for line in io.lines(file) do
        x, y, z, radius = string.match(line, "pos=<(-?%d+),(-?%d+),(-?%d+)>, r=(%d+)")
        table.insert(nanobots, {X=tonumber(x), Y=tonumber(y), Z=tonumber(z), Radius=tonumber(radius)})
    end
    return nanobots
end

function findStrongestNanobot(nanobots)
    local strongest = nanobots[1]
    for i=2, #nanobots do
        if nanobots[i].Radius > strongest.Radius then
            strongest = nanobots[i]
        end
    end
    return strongest
end

function countNanobotsInRange(nanobots, strongest)
    local count = 0
    for i=1, #nanobots do
        if manhattanDistance(nanobots[i], strongest) <= strongest.Radius then
            count = count + 1
        end
    end
    return count
end

function manhattanDistance(a, b)
    return math.abs(a.X - b.X) + math.abs(a.Y - b.Y) + math.abs(a.Z - b.Z)
end

nanobots = parseNanobots("input.txt")
strongest = findStrongestNanobot(nanobots)
inRangeCount = countNanobotsInRange(nanobots, strongest)

print(inRangeCount)
