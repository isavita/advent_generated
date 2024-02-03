
local file = io.open("input.txt", "r")
local startingNumbers = {}
for line in file:lines() do
    for number in string.gmatch(line, "%d+") do
        table.insert(startingNumbers, tonumber(number))
    end
end
file:close()

local lastSpoken = {}
local lastNumber
local nextNumber

for turn = 1, 2020 do
    if turn-1 < #startingNumbers then
        lastNumber = startingNumbers[turn]
        lastSpoken[lastNumber] = turn
    else
        local lastTurn = lastSpoken[lastNumber]
        if lastTurn and lastTurn ~= turn-1 then
            nextNumber = turn - 1 - lastTurn
        else
            nextNumber = 0
        end
        lastSpoken[lastNumber] = turn - 1
        lastNumber = nextNumber
    end
end

print(lastNumber)
