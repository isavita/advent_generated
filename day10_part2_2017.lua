
local file = io.open("input.txt", "r")
local input = file:read("*line")
file:close()

local lengths = {}
for i = 1, #input do
    table.insert(lengths, string.byte(input:sub(i, i)))
end
table.insert(lengths, 17)
table.insert(lengths, 31)
table.insert(lengths, 73)
table.insert(lengths, 47)
table.insert(lengths, 23)

local list = {}
for i = 0, 255 do
    list[i] = i
end

local currentPosition = 0
local skipSize = 0

for round = 1, 64 do
    for _, length in ipairs(lengths) do
        for i = 0, math.floor(length/2) - 1 do
            local start = (currentPosition + i) % 256
            local endPos = (currentPosition + length - 1 - i) % 256
            list[start], list[endPos] = list[endPos], list[start]
        end
        currentPosition = (currentPosition + length + skipSize) % 256
        skipSize = skipSize + 1
    end
end

local denseHash = {}
for i = 0, 255, 16 do
    local xor = 0
    for j = 0, 15 do
        xor = xor ~ list[i + j]
    end
    table.insert(denseHash, xor)
end

local hexHash = ""
for _, v in ipairs(denseHash) do
    hexHash = hexHash .. string.format("%02x", v)
end

print(hexHash)
