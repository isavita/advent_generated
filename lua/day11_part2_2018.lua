local inputFile = io.open("input.txt", "r")
local serial = tonumber(inputFile:read("*a"))
inputFile:close()

local gridSize = 300
local grid = {}
local sumTable = {}

-- Calculate power levels and initialize summed-area table
for y = 1, gridSize do
    grid[y] = {}
    sumTable[y] = {}
    for x = 1, gridSize do
        local rackID = x + 10
        local powerLevel = rackID * y
        powerLevel = powerLevel + serial
        powerLevel = powerLevel * rackID
        powerLevel = math.floor((powerLevel / 100) % 10)
        powerLevel = powerLevel - 5
        grid[y][x] = powerLevel

        -- Build summed-area table
        sumTable[y][x] = grid[y][x]
        if x > 1 then
            sumTable[y][x] = sumTable[y][x] + sumTable[y][x - 1]
        end
        if y > 1 then
            sumTable[y][x] = sumTable[y][x] + sumTable[y - 1][x]
        end
        if x > 1 and y > 1 then
            sumTable[y][x] = sumTable[y][x] - sumTable[y - 1][x - 1]
        end
    end
end

local maxPower = -math.huge
local maxX, maxY, maxSize

-- Compute the maximum power of any subgrid using the summed-area table
for size = 1, gridSize do
    for y = 1, gridSize - size + 1 do
        for x = 1, gridSize - size + 1 do
            local x1, y1 = x + size - 1, y + size - 1
            local totalPower = sumTable[y1][x1]

            if x > 1 then
                totalPower = totalPower - sumTable[y1][x - 1]
            end
            if y > 1 then
                totalPower = totalPower - sumTable[y - 1][x1]
            end
            if x > 1 and y > 1 then
                totalPower = totalPower + sumTable[y - 1][x - 1]
            end

            if totalPower > maxPower then
                maxPower = totalPower
                maxX, maxY, maxSize = x, y, size
            end
        end
    end
end

print(maxX .. "," .. maxY .. "," .. maxSize)