
local file = io.open("input.txt", "r")
if not file then
    print("Error opening file")
    return
end

local prev, current, count = 0, 0, 0
for line in file:lines() do
    current = tonumber(line)
    if prev ~= 0 and current > prev then
        count = count + 1
    end
    prev = current
end

file:close()

print(count)
