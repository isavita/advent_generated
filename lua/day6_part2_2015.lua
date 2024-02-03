
local gridSize = 1000
local grid = {}

for i = 1, gridSize do
    grid[i] = {}
    for j = 1, gridSize do
        grid[i][j] = 0
    end
end

for line in io.lines("input.txt") do
    local instruction = line
    local parts = {}
    for part in instruction:gmatch("%S+") do
        table.insert(parts, part)
    end

    local startX, startY = parts[#parts-2]:match("(%d+),(%d+)")
    local endX, endY = parts[#parts]:match("(%d+),(%d+)")

    for x = tonumber(startX), tonumber(endX) do
        for y = tonumber(startY), tonumber(endY) do
            if instruction:find("turn on") then
                grid[x][y] = grid[x][y] + 1
            elseif instruction:find("turn off") then
                if grid[x][y] > 0 then
                    grid[x][y] = grid[x][y] - 1
                end
            elseif instruction:find("toggle") then
                grid[x][y] = grid[x][y] + 2
            end
        end
    end
end

local brightness = 0
for i = 1, gridSize do
    for j = 1, gridSize do
        brightness = brightness + grid[i][j]
    end
end

print(brightness)
