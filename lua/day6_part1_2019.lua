-- Read input from "input.txt"
local file = io.open("input.txt", "r")
local lines = {}
for line in file:lines() do
    table.insert(lines, line)
end
file:close()

-- Create the orbit map
local orbit_map = {}
for _, line in ipairs(lines) do
    local parts = {}
    for part in string.gmatch(line, "([^)]+)") do
        table.insert(parts, part)
    end
    local center, orbiter = parts[1], parts[2]
    if not orbit_map[center] then
        orbit_map[center] = {}
    end
    table.insert(orbit_map[center], orbiter)
end

-- Count the orbits
local function count_orbits(orbit_map, start, depth)
    local orbits = orbit_map[start]
    if not orbits then
        return depth
    end
    local count = depth
    for _, orbit in ipairs(orbits) do
        count = count + count_orbits(orbit_map, orbit, depth + 1)
    end
    return count
end

local total_orbits = count_orbits(orbit_map, "COM", 0)
print(total_orbits)