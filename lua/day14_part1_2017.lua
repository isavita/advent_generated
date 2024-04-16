function reverseSection(arr, start, length)
    local n = #arr
    for i = 0, length // 2 - 1 do
        local a, b = (start + i - 1) % n + 1, (start + length - i - 2) % n + 1
        arr[a], arr[b] = arr[b], arr[a]
    end
end

function knotHash(input)
    local lengths = {}
    for i = 1, #input do
        table.insert(lengths, string.byte(input, i))
    end
    table.insert(lengths, 17)
    table.insert(lengths, 31)
    table.insert(lengths, 73)
    table.insert(lengths, 47)
    table.insert(lengths, 23)

    local list = {}
    for i = 1, 256 do
        list[i] = i - 1
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
    for i = 1, 16 do
        local xor = 0
        for j = 1, 16 do
            xor = xor ~ list[(i - 1) * 16 + j]
        end
        table.insert(denseHash, xor)
    end

    local hexHash = {}
    for _, v in ipairs(denseHash) do
        table.insert(hexHash, string.format("%02x", v))
    end
    return table.concat(hexHash)
end

function hexToBinary(hexStr)
    local binaryStr = ""
    for i = 1, #hexStr do
        local hexDigit = string.sub(hexStr, i, i)
        local val = tonumber(hexDigit, 16)
        local binaryVal = ""
        for j = 1, 4 do
            binaryVal = tostring(val % 2) .. binaryVal
            val = math.floor(val / 2)
        end
        binaryStr = binaryStr .. binaryVal
    end
    return binaryStr
end

local file = io.open("input.txt", "r")
local keyString = file:read("*a")
file:close()
keyString = keyString:gsub("%s+", "")

local totalUsed = 0
for i = 0, 127 do
    local rowKey = keyString .. "-" .. tostring(i)
    local hash = knotHash(rowKey)
    local binaryRow = hexToBinary(hash)

    for j = 1, #binaryRow do
        if string.sub(binaryRow, j, j) == '1' then
            totalUsed = totalUsed + 1
        end
    end
end

print(totalUsed)