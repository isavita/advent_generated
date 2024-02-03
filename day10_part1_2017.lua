
-- Read input lengths from a file
local file = io.open("input.txt", "r")
local lengthsStr = {}
if file then
    local contents = file:read("*all")
    lengthsStr = string.gmatch(contents, "%d+")
    file:close()
end

-- Initialize variables
local list = {}
for i = 1, 256 do
    list[i] = i - 1
end
local currentPosition = 0
local skipSize = 0

-- Perform the knot-tying operations
for length in lengthsStr do
    length = tonumber(length)
    -- Reverse the elements
    for i = 0, math.floor(length / 2) - 1 do
        local start = (currentPosition + i) % 256 + 1
        local endPos = (currentPosition + length - 1 - i) % 256 + 1
        list[start], list[endPos] = list[endPos], list[start]
    end

    -- Move the current position and increase the skip size
    currentPosition = (currentPosition + length + skipSize) % 256
    skipSize = skipSize + 1
end

-- Multiply the first two numbers in the list
local result = list[1] * list[2]
print(result)
