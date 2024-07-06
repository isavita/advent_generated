local math = require("math")

-- Read input from file
local function read_input()
    local file = io.open("input.txt", "r")
    if not file then error("Could not open input file") end
    local asteroids = {}
    local y = 0
    for line in file:lines() do
        for x = 1, #line do
            if line:sub(x, x) == '#' then
                table.insert(asteroids, {x = x - 1, y = y})
            end
        end
        y = y + 1
    end
    file:close()
    return asteroids
end

-- Calculate angle between two points
local function calc_angle(x1, y1, x2, y2)
    return (math.atan2(y2 - y1, x2 - x1) + math.pi * 2.5) % (math.pi * 2)
end

-- Calculate distance between two points
local function calc_distance(x1, y1, x2, y2)
    return (x2 - x1)^2 + (y2 - y1)^2  -- No need for square root for comparison
end

-- Find the best monitoring station
local function find_best_station(asteroids)
    local best_count, best_asteroid = 0, nil
    for _, a1 in ipairs(asteroids) do
        local angles = {}
        for _, a2 in ipairs(asteroids) do
            if a1 ~= a2 then
                local angle = calc_angle(a1.x, a1.y, a2.x, a2.y)
                angles[angle] = true
            end
        end
        local count = 0
        for _ in pairs(angles) do count = count + 1 end
        if count > best_count then
            best_count, best_asteroid = count, a1
        end
    end
    return best_asteroid, best_count
end

-- Vaporize asteroids
local function vaporize_asteroids(asteroids, station)
    local angle_groups = {}
    for _, asteroid in ipairs(asteroids) do
        if asteroid ~= station then
            local angle = calc_angle(station.x, station.y, asteroid.x, asteroid.y)
            local distance = calc_distance(station.x, station.y, asteroid.x, asteroid.y)
            if not angle_groups[angle] then angle_groups[angle] = {} end
            table.insert(angle_groups[angle], {asteroid = asteroid, distance = distance})
        end
    end

    for _, group in pairs(angle_groups) do
        table.sort(group, function(a, b) return a.distance < b.distance end)
    end

    local angles = {}
    for angle in pairs(angle_groups) do
        table.insert(angles, angle)
    end
    table.sort(angles)

    local vaporized = {}
    local angle_index = 1
    while #vaporized < 200 and #vaporized < #asteroids - 1 do
        local current_angle = angles[angle_index]
        local group = angle_groups[current_angle]
        if #group > 0 then
            table.insert(vaporized, table.remove(group, 1).asteroid)
        end
        angle_index = angle_index % #angles + 1
    end

    return vaporized
end

-- Main function
local function main()
    local asteroids = read_input()
    local station, _ = find_best_station(asteroids)
    local vaporized = vaporize_asteroids(asteroids, station)
    
    local target = vaporized[200]
    if target then
        local result = target.x * 100 + target.y
        print("The 200th asteroid to be vaporized is at " .. target.x .. "," .. target.y)
        print("Result: " .. result)
    else
        print("Not enough asteroids to vaporize 200")
    end
end

main()
