
local file = io.open("input.txt", "r")
if not file then
    print("Error opening file")
    return
end

local input = file:read("*line")
file:close()

local fishes = {0, 0, 0, 0, 0, 0, 0, 0, 0}
for fishStr in input:gmatch("%d+") do
    local fish = tonumber(fishStr)
    fishes[fish + 1] = fishes[fish + 1] + 1
end

for day = 1, 80 do
    local newFish = fishes[1]
    for i = 1, 8 do
        fishes[i] = fishes[i + 1]
    end
    fishes[7] = fishes[7] + newFish
    fishes[9] = newFish
end

local totalFish = 0
for _, fish in ipairs(fishes) do
    totalFish = totalFish + fish
end

print(totalFish)
