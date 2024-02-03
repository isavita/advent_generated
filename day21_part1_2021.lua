
local file = io.open("input.txt", "r")
local lines = {}
for line in file:lines() do
    table.insert(lines, line)
end
file:close()

local player1Start = tonumber(string.sub(lines[1], 29))
local player2Start = tonumber(string.sub(lines[2], 29))
local player1Pos = player1Start
local player2Pos = player2Start
local player1Score = 0
local player2Score = 0
local dieRoll = 1
local rollCount = 0

while true do
    -- Player 1
    local rolls = dieRoll % 100 + (dieRoll + 1) % 100 + (dieRoll + 2) % 100
    rollCount = rollCount + 3
    dieRoll = dieRoll + 3
    player1Pos = (player1Pos + rolls - 1) % 10 + 1
    player1Score = player1Score + player1Pos

    if player1Score >= 1000 then
        print(player2Score * rollCount)
        break
    end

    -- Player 2
    rolls = dieRoll % 100 + (dieRoll + 1) % 100 + (dieRoll + 2) % 100
    rollCount = rollCount + 3
    dieRoll = dieRoll + 3
    player2Pos = (player2Pos + rolls - 1) % 10 + 1
    player2Score = player2Score + player2Pos

    if player2Score >= 1000 then
        print(player1Score * rollCount)
        break
    end
end
