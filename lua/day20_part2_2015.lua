
local file = io.open("input.txt", "r")
local input = file:read("*line")
file:close()

local target = tonumber(input) / 11
local houses = {}
for i = 1, target+1 do
    houses[i] = 0
end

for elf = 1, target do
    for house = elf, elf*50, elf do
        if house <= target then
            houses[house] = houses[house] + elf
        end
    end
end

for houseNumber, presents in ipairs(houses) do
    if presents >= target then
        print(houseNumber)
        break
    end
end
