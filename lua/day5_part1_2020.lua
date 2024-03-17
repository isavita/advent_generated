function binaryToInt(binaryStr)
    local result = 0
    for i = 1, #binaryStr do
        if binaryStr:sub(i, i) == "1" then
            result = result + 2 ^ (#binaryStr - i)
        end
    end
    return result
end

local file = io.open("input.txt", "r")
if not file then
    print("Error opening file")
    return
end

local maxSeatID = 0
for line in file:lines() do
    local pass = line:gsub("F", "0"):gsub("B", "1"):gsub("L", "0"):gsub("R", "1")
    local seatID = binaryToInt(pass:sub(1, 7)) * 8 + binaryToInt(pass:sub(8))
    if seatID > maxSeatID then
        maxSeatID = seatID
    end
end

print(maxSeatID)
file:close()