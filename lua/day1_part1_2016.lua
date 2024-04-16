function abs(x)
    if x < 0 then
        return -x
    end
    return x
end

local directions = {{0, 1}, {1, 0}, {0, -1}, {-1, 0}} -- North, East, South, West
local pos = {x = 0, y = 0, dirIndex = 1} -- Starting facing North

local file = io.open("input.txt", "r")
local instructions = {}
for line in file:lines() do
    for instruction in string.gmatch(line, "[^, ]+") do
        table.insert(instructions, instruction)
    end
end
file:close()

for _, instruction in ipairs(instructions) do
    local turn = instruction:sub(1, 1)
    local blocks = tonumber(instruction:sub(2))

    if turn == "R" then
        pos.dirIndex = (pos.dirIndex % 4) + 1
    else
        pos.dirIndex = (pos.dirIndex - 2 + 4) % 4 + 1
    end

    pos.x = pos.x + directions[pos.dirIndex][1] * blocks
    pos.y = pos.y + directions[pos.dirIndex][2] * blocks
end

print(abs(pos.x) + abs(pos.y))