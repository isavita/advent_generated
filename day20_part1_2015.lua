
local file = io.open("input.txt", "r")
local input = file:read("*line")
file:close()

local target = tonumber(input) / 10

local houses = {}
for i = 1, target+1 do
    houses[i] = 0
end

for elf = 1, target do
    for house = elf, target, elf do
        houses[house] = houses[house] + elf
    end
end

for houseNumber, presents in ipairs(houses) do
    if presents >= target then
        print(houseNumber)
        break
    end
end
