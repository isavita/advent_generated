-- Read input from "input.txt"
local file = io.open("input.txt", "r")
local input = tonumber(file:read("*l"))
file:close()

local scoreboard = {3, 7}
local elf1, elf2 = 0, 1

while #scoreboard < input + 10 do
    local newScore = scoreboard[elf1 + 1] + scoreboard[elf2 + 1]
    if newScore >= 10 then
        table.insert(scoreboard, math.floor(newScore / 10))
    end
    table.insert(scoreboard, newScore % 10)

    elf1 = (elf1 + scoreboard[elf1 + 1] + 1) % #scoreboard
    elf2 = (elf2 + scoreboard[elf2 + 1] + 1) % #scoreboard
end

for i = input + 1, input + 10 do
    io.write(scoreboard[i])
end
io.write("\n")