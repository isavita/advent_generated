local file = io.open("input.txt", "r")
local vals = {}

for line in file:lines() do
    if line ~= "" then
        table.insert(vals, tonumber(line))
    end
end

file:close()

local prevSum = vals[1] + vals[2] + vals[3]
local count = 0

for i = 4, #vals do
    local currSum = vals[i-2] + vals[i-1] + vals[i]
    if currSum > prevSum then
        count = count + 1
    end
    prevSum = currSum
end

print(count)