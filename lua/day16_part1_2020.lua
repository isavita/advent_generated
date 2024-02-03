
local rules = {}
local scanningRules = true
local errorRate = 0

local function isValid(value, ranges)
    for _, rng in ipairs(ranges) do
        if value >= rng[1] and value <= rng[2] then
            return true
        end
    end
    return false
end

local function toInt(s)
    return tonumber(s)
end

local function isValidForAnyRule(value, rules)
    for _, rule in ipairs(rules) do
        if isValid(value, rule.ranges) then
            return true
        end
    end
    return false
end

local file = io.open("input.txt", "r")
if not file then
    print("Error opening file")
    return
end

for line in file:lines() do
    if line ~= "" then
        if string.match(line, "your ticket:") or string.match(line, "nearby tickets:") then
            scanningRules = false
        end

        if scanningRules then
            local name, range1_min, range1_max, range2_min, range2_max = string.match(line, "^([^:]+): (%d+)-(%d+) or (%d+)-(%d+)$")
            if name then
                local range1 = {toInt(range1_min), toInt(range1_max)}
                local range2 = {toInt(range2_min), toInt(range2_max)}
                table.insert(rules, {name = name, ranges = {range1, range2}})
            end
        else
            for value in string.gmatch(line, "%d+") do
                local val = toInt(value)
                if not isValidForAnyRule(val, rules) then
                    errorRate = errorRate + val
                end
            end
        end
    end
end

print(errorRate)
