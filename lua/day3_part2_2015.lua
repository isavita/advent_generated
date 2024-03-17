local file = io.open("input.txt", "r")
local directions = file:read("*all")
file:close()

local visitedHouses = {}
local xSanta, ySanta = 0, 0
local xRobo, yRobo = 0, 0
local isSantaTurn = true

visitedHouses["0,0"] = true

for i = 1, #directions do
    local dir = directions:sub(i, i)
    local x, y

    if isSantaTurn then
        x, y = xSanta, ySanta
    else
        x, y = xRobo, yRobo
    end

    if dir == "^" then
        y = y + 1
    elseif dir == "v" then
        y = y - 1
    elseif dir == ">" then
        x = x + 1
    elseif dir == "<" then
        x = x - 1
    end

    visitedHouses[x .. "," .. y] = true
    if isSantaTurn then
        xSanta, ySanta = x, y
    else
        xRobo, yRobo = x, y
    end
    isSantaTurn = not isSantaTurn
end

local count = 0
for k, v in pairs(visitedHouses) do
    count = count + 1
end

print(count)