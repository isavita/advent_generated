
function manhattanDistance(x1, y1, x2, y2)
    return math.abs(x1 - x2) + math.abs(y1 - y2)
end

function parseInput(filename)
    local sensors = {}
    for line in io.lines(filename) do
        local sx, sy, bx, by = line:match("Sensor at x=(-?%d+), y=(-?%d+): closest beacon is at x=(-?%d+), y=(-?%d+)")
        sx, sy, bx, by = tonumber(sx), tonumber(sy), tonumber(bx), tonumber(by)
        local distance = manhattanDistance(sx, sy, bx, by)
        table.insert(sensors, {x = sx, y = sy, bx = bx, by = by, distance = distance})
    end
    return sensors
end

function partOne(sensors, targetY)
    local noBeaconPositions = {}
    local beaconPositions = {}

    for _, sensor in ipairs(sensors) do
        local dy = math.abs(sensor.y - targetY)
        if dy <= sensor.distance then
            local width = sensor.distance - dy
            for x = sensor.x - width, sensor.x + width do
                noBeaconPositions[x] = true
            end
        end
        if sensor.by == targetY then
            beaconPositions[sensor.bx] = true
        end
    end

    local count = 0
    for x in pairs(noBeaconPositions) do
        if not beaconPositions[x] then
            count = count + 1
        end
    end
    return count
end

function partTwo(sensors, maxCoord)
    for y = 0, maxCoord do
        local intervals = {}
        for _, sensor in ipairs(sensors) do
            local dy = math.abs(sensor.y - y)
            if dy <= sensor.distance then
                local width = sensor.distance - dy
                local left = sensor.x - width
                local right = sensor.x + width
                table.insert(intervals, {left = math.max(0, left), right = math.min(maxCoord, right)})
            end
        end

        table.sort(intervals, function(a, b) return a.left < b.left end)

        local merged = {intervals[1]}
        for i = 2, #intervals do
            local last = merged[#merged]
            local current = intervals[i]
            if current.left > last.right + 1 then
                return (last.right + 1) * 4000000 + y
            end
            merged[#merged] = {left = last.left, right = math.max(last.right, current.right)}
        end
    end
end

local sensors = parseInput("input.txt")
print("Part One:", partOne(sensors, 2000000))
print("Part Two:", partTwo(sensors, 4000000))
