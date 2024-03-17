local file = io.open("input.txt", "r")
if not file then
    error("Failed to open input.txt")
end

local heightmap = {}
for line in file:lines() do
    local row = {}
    for char in line:gmatch(".") do
        table.insert(row, tonumber(char))
    end
    table.insert(heightmap, row)
end
file:close()

local function is_low_point(heightmap, x, y)
    local height = heightmap[y][x]
    if x > 1 and heightmap[y][x-1] <= height then
        return false
    end
    if x < #heightmap[y] and heightmap[y][x+1] <= height then
        return false
    end
    if y > 1 and heightmap[y-1][x] <= height then
        return false
    end
    if y < #heightmap and heightmap[y+1][x] <= height then
        return false
    end
    return true
end

local total_risk_level = 0
for y, row in ipairs(heightmap) do
    for x, height in ipairs(row) do
        if is_low_point(heightmap, x, y) then
            total_risk_level = total_risk_level + 1 + height
        end
    end
end

print(total_risk_level)