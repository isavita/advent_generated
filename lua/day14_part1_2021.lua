-- Read input from file
local file = io.open("input.txt", "r")
local polymer = file:read("*l")
local rules = {}
for line in file:lines() do
    if line ~= "" then
        local parts = {}
        for part in line:gmatch("%S+") do
            table.insert(parts, part)
        end
        rules[parts[1]] = parts[3]
    end
end
file:close()

-- Apply insertion for 10 steps
for step = 1, 10 do
    local newPolymer = ""
    for i = 1, #polymer - 1 do
        newPolymer = newPolymer .. polymer:sub(i, i)
        local pair = polymer:sub(i, i + 1)
        if rules[pair] then
            newPolymer = newPolymer .. rules[pair]
        end
    end
    newPolymer = newPolymer .. polymer:sub(#polymer, #polymer)
    polymer = newPolymer
end

-- Count elements
local counts = {}
for i = 1, #polymer do
    local c = polymer:sub(i, i)
    if not counts[c] then
        counts[c] = 0
    end
    counts[c] = counts[c] + 1
end

-- Find minimum and maximum
local min, max = math.huge, 0
for _, count in pairs(counts) do
    if count < min then
        min = count
    end
    if count > max then
        max = count
    end
end

-- Print the answer
print(max - min)