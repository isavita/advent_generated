
local function maxInt(a, b)
    return a > b and a or b
end

local function minInt(a, b)
    return a < b and a or b
end

local function parseInput(input)
    local cubes = {}
    for line in input:gmatch("[^\n]+") do
        local state, coords = line:match("(%w+) (.+)")
        local x1, x2, y1, y2, z1, z2 = coords:match("x=(-?%d+)%.%.(-?%d+),y=(-?%d+)%.%.(-?%d+),z=(-?%d+)%.%.(-?%d+)")
        
        x1, x2 = tonumber(x1), tonumber(x2)
        y1, y2 = tonumber(y1), tonumber(y2)
        z1, z2 = tonumber(z1), tonumber(z2)
        
        table.insert(cubes, {
            isOn = state == "on",
            x1 = x1, x2 = x2,
            y1 = y1, y2 = y2,
            z1 = z1, z2 = z2
        })
    end
    return cubes
end

local function getCubeVolume(cube)
    local vol = (cube.x2 - cube.x1 + 1) * (cube.y2 - cube.y1 + 1) * (cube.z2 - cube.z1 + 1)
    return cube.isOn and vol or -vol
end

local function getIntersection(c1, c2)
    local x1 = maxInt(c1.x1, c2.x1)
    local x2 = minInt(c1.x2, c2.x2)
    local y1 = maxInt(c1.y1, c2.y1)
    local y2 = minInt(c1.y2, c2.y2)
    local z1 = maxInt(c1.z1, c2.z1)
    local z2 = minInt(c1.z2, c2.z2)

    if x1 > x2 or y1 > y2 or z1 > z2 then
        return nil
    end

    local intersectionState
    if c1.isOn and c2.isOn then
        intersectionState = false
    elseif not c1.isOn and not c2.isOn then
        intersectionState = true
    else
        intersectionState = c2.isOn
    end

    return {
        isOn = intersectionState,
        x1 = x1, x2 = x2,
        y1 = y1, y2 = y2,
        z1 = z1, z2 = z2
    }
end

local function solve(input)
    local cubes = parseInput(input)
    local finalList = {}

    for _, c in ipairs(cubes) do
        local toAdd = {}

        for _, finalCube in ipairs(finalList) do
            local intersection = getIntersection(finalCube, c)
            if intersection then
                table.insert(toAdd, intersection)
            end
        end

        if c.isOn then
            table.insert(toAdd, c)
        end

        for _, cube in ipairs(toAdd) do
            table.insert(finalList, cube)
        end
    end

    local total = 0
    for _, c in ipairs(finalList) do
        total = total + getCubeVolume(c)
    end

    return total
end

local function main()
    local file = io.open("input.txt", "r")
    local input = file:read("*all"):gsub("%s+$", "")
    file:close()

    local result = solve(input)
    print(result)
end

main()
