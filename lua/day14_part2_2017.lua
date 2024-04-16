function reverseSection(arr, start, length)
    local n = #arr
    local i, j = start, start + length - 1
    while i < j do
        arr[(i-1)%n+1], arr[(j-1)%n+1] = arr[(j-1)%n+1], arr[(i-1)%n+1]
        i = i + 1
        j = j - 1
    end
end

function knotHash(input)
    local lengths = {}
    for i = 1, #input do
        table.insert(lengths, string.byte(input, i))
    end
    for _, v in ipairs({17, 31, 73, 47, 23}) do
        table.insert(lengths, v)
    end

    local list = {}
    for i = 0, 255 do
        table.insert(list, i)
    end

    local position, skip = 1, 0
    for round = 1, 64 do
        for _, length in ipairs(lengths) do
            reverseSection(list, position, length)
            position = (position + length + skip - 1) % #list + 1
            skip = skip + 1
        end
    end

    local denseHash = {}
    for i = 0, 15 do
        local xor = 0
        for j = 1, 16 do
            xor = xor ~ list[i * 16 + j]
        end
        table.insert(denseHash, xor)
    end

    local hexHash = ""
    for _, v in ipairs(denseHash) do
        hexHash = hexHash .. string.format("%02x", v)
    end
    return hexHash
end

function hexDigitToBinary(hexDigit)
    local val = tonumber(hexDigit, 16)
    local binary = ""
    for i = 3, 0, -1 do
        binary = binary .. tostring(math.floor(val / 2^i) % 2)
    end
    return binary
end

function hexToBinary(hexStr)
    local binaryStr = ""
    for i = 1, #hexStr do
        local hexDigit = string.sub(hexStr, i, i)
        binaryStr = binaryStr .. hexDigitToBinary(hexDigit)
    end
    return binaryStr
end

function dfs(x, y, grid)
    if x < 1 or x > 128 or y < 1 or y > 128 or grid[x][y] ~= 1 then
        return
    end
    grid[x][y] = 0
    dfs(x-1, y, grid)
    dfs(x+1, y, grid)
    dfs(x, y-1, grid)
    dfs(x, y+1, grid)
end

local file = io.open("input.txt", "r")
local keyString = file:read("*a"):gsub("%s+", "")
file:close()

local grid = {}
local totalUsed = 0
local regions = 0

for i = 0, 127 do
    grid[i+1] = {}
    local rowKey = keyString .. "-" .. tostring(i)
    local hash = knotHash(rowKey)
    local binaryRow = hexToBinary(hash)

    for j = 1, 128 do
        local bit = string.sub(binaryRow, j, j)
        if bit == '1' then
            grid[i+1][j] = 1
            totalUsed = totalUsed + 1
        else
            grid[i+1][j] = 0
        end
    end
end

for i = 1, 128 do
    for j = 1, 128 do
        if grid[i][j] == 1 then
            regions = regions + 1
            dfs(i, j, grid)
        end
    end
end

print("Total used squares:", totalUsed)
print("Number of regions:", regions)