
local file = io.open("input.txt", "r")
local steps = tonumber(file:read("*line"))
file:close()

local currentPos = 0
local valueAfterZero = 0

for i = 1, 50000000 do
    currentPos = (currentPos + steps) % i
    if currentPos == 0 then
        valueAfterZero = i
    end
    currentPos = currentPos + 1
end

print(valueAfterZero)
