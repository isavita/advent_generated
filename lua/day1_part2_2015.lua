
local file = io.open("input.txt", "r")
local input = file:read("*all")
file:close()

local floor = 0
local position = 0
for i = 1, #input do
    local c = input:sub(i, i)
    if c == "(" then
        floor = floor + 1
    elseif c == ")" then
        floor = floor - 1
    end
    if floor == -1 then
        position = i
        break
    end
end

print(position)
