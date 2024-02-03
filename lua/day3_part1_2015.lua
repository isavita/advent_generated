
local file = io.open("input.txt", "r")
local directions = file:read("*all")
file:close()

local visitedHouses = {}
local x, y = 0, 0

visitedHouses[x .. "," .. y] = true

for i = 1, #directions do
    local dir = directions:sub(i, i)
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
end

local count = 0
for k, v in pairs(visitedHouses) do
    count = count + 1
end

print(count)
