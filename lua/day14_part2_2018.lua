local file = io.open("input.txt", "r")
local input = file:read("*a")
file:close()

local scoreboard = {3, 7}
local elf1, elf2 = 0, 1
local inputSequence = {}
for i = 1, #input do
    inputSequence[i] = tonumber(input:sub(i, i))
end

local function checkSequence(scoreboard, sequence)
    if #scoreboard < #sequence then
        return false
    end
    local start = #scoreboard - #sequence + 1
    for i = 1, #sequence do
        if scoreboard[start + i - 1] ~= sequence[i] then
            return false
        end
    end
    return true
end

while true do
    local newScore = scoreboard[elf1 + 1] + scoreboard[elf2 + 1]
    if newScore >= 10 then
        scoreboard[#scoreboard + 1] = math.floor(newScore / 10)
        if checkSequence(scoreboard, inputSequence) then
            break
        end
    end
    scoreboard[#scoreboard + 1] = newScore % 10
    if checkSequence(scoreboard, inputSequence) then
        break
    end

    elf1 = (elf1 + scoreboard[elf1 + 1] + 1) % #scoreboard
    elf2 = (elf2 + scoreboard[elf2 + 1] + 1) % #scoreboard
end

print(#scoreboard - #inputSequence)