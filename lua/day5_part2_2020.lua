-- Read input from file
local file = io.open("input.txt", "r")
local lines = {}
for line in file:lines() do
    table.insert(lines, line)
end
file:close()

-- Helper function to convert binary string to integer
local function binaryToInt(binaryStr)
    local result = 0
    for i = 1, #binaryStr do
        if binaryStr:sub(i, i) == "1" then
            result = result | (1 << (#binaryStr - i))
        end
    end
    return result
end

-- Decode seat ID
local function decode(pass)
    local row = binaryToInt(pass:sub(1, 7))
    local column = binaryToInt(pass:sub(8))
    return row * 8 + column
end

-- Process input and find the missing seat ID
local seatIDs = {}
for _, pass in ipairs(lines) do
    pass = pass:gsub("F", "0"):gsub("B", "1"):gsub("L", "0"):gsub("R", "1")
    local seatID = decode(pass)
    table.insert(seatIDs, seatID)
end
table.sort(seatIDs)

for i = 1, #seatIDs - 1 do
    if seatIDs[i + 1] ~= seatIDs[i] + 1 then
        print(seatIDs[i] + 1)
        break
    end
end