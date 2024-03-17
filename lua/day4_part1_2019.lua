local file = io.open("input.txt", "r")
local rangeStr = file:read()
file:close()

local parts = {}
for part in string.gmatch(rangeStr, "%d+") do
    table.insert(parts, tonumber(part))
end

local start = parts[1]
local endNum = parts[2]
local count = 0

function hasDoubleAndIncreasingDigits(s)
    local hasDouble = false
    for i = 1, #s - 1 do
        if s:sub(i, i) == s:sub(i + 1, i + 1) then
            hasDouble = true
        end
        if s:sub(i, i) > s:sub(i + 1, i + 1) then
            return false
        end
    end
    return hasDouble
end

for i = start, endNum do
    local s = tostring(i)
    if hasDoubleAndIncreasingDigits(s) then
        count = count + 1
    end
end

print(count)