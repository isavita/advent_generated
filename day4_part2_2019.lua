
function isValidPassword(password)
    local s = tostring(password)
    local hasDouble = false

    for i = 1, #s - 1 do
        if tonumber(s:sub(i, i)) > tonumber(s:sub(i + 1, i + 1)) then
            return false
        end
        if s:sub(i, i) == s:sub(i + 1, i + 1) then
            if (i == 1 or s:sub(i, i) ~= s:sub(i - 1, i - 1)) and (i + 2 > #s or s:sub(i, i) ~= s:sub(i + 2, i + 2)) then
                hasDouble = true
            end
        end
    end

    return hasDouble
end

local file = io.open("input.txt", "r")
local rangeStr = file:read("*line")
file:close()

local ranges = {}
for range in string.gmatch(rangeStr, "%d+") do
    table.insert(ranges, tonumber(range))
end

local start = ranges[1]
local endRange = ranges[2]

local count = 0
for i = start, endRange do
    if isValidPassword(i) then
        count = count + 1
    end
end

print(count)
