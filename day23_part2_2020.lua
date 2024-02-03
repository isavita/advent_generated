
local totalCups = 1000000
local totalMoves = 10000000

local file = io.open("input.txt", "r")
local input = file:read("*line")
file:close()

local cups = {}
local lastCup

for i = 1, #input do
    local cup = tonumber(input:sub(i, i))
    if i > 1 then
        cups[lastCup] = cup
    end
    lastCup = cup
end

for i = #input + 1, totalCups do
    cups[lastCup] = i
    lastCup = i
end
cups[lastCup] = tonumber(input:sub(1, 1))

local currentCup = tonumber(input:sub(1, 1))
for i = 1, totalMoves do
    local pickup1 = cups[currentCup]
    local pickup2 = cups[pickup1]
    local pickup3 = cups[pickup2]

    cups[currentCup] = cups[pickup3]

    local destinationCup = currentCup - 1
    if destinationCup == 0 then
        destinationCup = totalCups
    end
    while destinationCup == pickup1 or destinationCup == pickup2 or destinationCup == pickup3 do
        destinationCup = destinationCup - 1
        if destinationCup == 0 then
            destinationCup = totalCups
        end
    end

    cups[pickup3] = cups[destinationCup]
    cups[destinationCup] = pickup1

    currentCup = cups[currentCup]
end

local cup1 = cups[1]
local cup2 = cups[cup1]
print(cup1 * cup2)
