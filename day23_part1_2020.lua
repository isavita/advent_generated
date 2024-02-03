
local file = io.open("input.txt", "r")
local input = file:read()
file:close()

local cups = {}
local currentCup = 0
for i = 1, #input do
    local cup = tonumber(input:sub(i, i))
    if i == 1 then
        currentCup = cup
    end
    if i < #input then
        local nextCup = tonumber(input:sub(i+1, i+1))
        cups[cup] = nextCup
    end
end
local firstCup = tonumber(input:sub(1, 1))
local lastCup = tonumber(input:sub(#input, #input))
cups[lastCup] = firstCup

for i = 1, 100 do
    local pickup1 = cups[currentCup]
    local pickup2 = cups[pickup1]
    local pickup3 = cups[pickup2]

    cups[currentCup] = cups[pickup3]

    local destinationCup = currentCup - 1
    if destinationCup < 1 then
        destinationCup = #input
    end
    while destinationCup == pickup1 or destinationCup == pickup2 or destinationCup == pickup3 do
        destinationCup = destinationCup - 1
        if destinationCup < 1 then
            destinationCup = #input
        end
    end

    cups[pickup3] = cups[destinationCup]
    cups[destinationCup] = pickup1

    currentCup = cups[currentCup]
end

local cup = cups[1]
while cup ~= 1 do
    io.write(cup)
    cup = cups[cup]
    if cup == 1 then
        break
    end
end
print()
