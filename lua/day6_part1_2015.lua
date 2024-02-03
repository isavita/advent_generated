
local gridSize = 1000

local grid = {}
for i = 1, gridSize do
    grid[i] = {}
    for j = 1, gridSize do
        grid[i][j] = false
    end
end

local file = io.open("input.txt", "r")
if not file then
    print("Error opening file")
    os.exit()
end

for line in file:lines() do
    local parts = {}
    for part in line:gmatch("%S+") do
        table.insert(parts, part)
    end

    local startX, startY = parts[#parts-2]:match("(%d+),(%d+)")
    local endX, endY = parts[#parts]:match("(%d+),(%d+)")
    startX, startY, endX, endY = tonumber(startX), tonumber(startY), tonumber(endX), tonumber(endY)

    for x = startX, endX do
        for y = startY, endY do
            if string.find(line, "turn on") then
                grid[x][y] = true
            elseif string.find(line, "turn off") then
                grid[x][y] = false
            elseif string.find(line, "toggle") then
                grid[x][y] = not grid[x][y]
            end
        end
    end
end

file:close()

local count = 0
for i = 1, gridSize do
    for j = 1, gridSize do
        if grid[i][j] then
            count = count + 1
        end
    end
end

print(count)
