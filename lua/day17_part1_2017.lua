-- Read the input from the file
local file = io.open("input.txt", "r")
local steps = tonumber(file:read("*l"))
file:close()

local buffer = {0}
local currentPos = 1

for i = 1, 2017 do
    currentPos = (currentPos + steps) % #buffer + 1
    table.insert(buffer, currentPos + 1, i)
end

for i, val in ipairs(buffer) do
    if val == 2017 then
        print(buffer[(i % #buffer) + 1])
        break
    end
end