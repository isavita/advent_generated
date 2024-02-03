
local file = io.open("input.txt", "r")
local input = file:read("*all")
file:close()

input = input:gsub("%s+", "")

local sum = 0

for i = 1, #input do
    local next = (i % #input) + 1
    if input:sub(i, i) == input:sub(next, next) then
        sum = sum + tonumber(input:sub(i, i))
    end
end

print(sum)
