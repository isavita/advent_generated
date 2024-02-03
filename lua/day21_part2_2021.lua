
local file = io.open("input.txt", "r")
local input = file:read("*all")
file:close()

function solve(input)
    local positions = parseInput(input)
    local w1, w2 = play({positions[1], positions[2]}, {0, 0}, 3, true, {})

    if w1 > w2 then
        return w1
    end
    return w2
end

function play(positions, scores, rollsLeftInTurn, isPlayer1sTurn, memo)
    local key = table.concat(positions, ",") .. table.concat(scores, ",") .. rollsLeftInTurn .. tostring(isPlayer1sTurn)
    if memo[key] then
        return memo[key][1], memo[key][2]
    end

    local playerIndex = 2
    if isPlayer1sTurn then
        playerIndex = 1
    end

    local scoresCopy = {scores[1], scores[2]}
    if rollsLeftInTurn == 0 then
        scoresCopy[playerIndex] = scoresCopy[playerIndex] + positions[playerIndex]

        if scoresCopy[playerIndex] >= 21 then
            if playerIndex == 1 then
                return 1, 0
            end
            return 0, 1
        end

        isPlayer1sTurn = not isPlayer1sTurn
        rollsLeftInTurn = 3

        playerIndex = playerIndex % 2 + 1
    end

    local wins1, wins2 = 0, 0
    for roll = 1, 3 do
        local positionsCopy = {positions[1], positions[2]}
        positionsCopy[playerIndex] = positionsCopy[playerIndex] + roll
        if positionsCopy[playerIndex] > 10 then
            positionsCopy[playerIndex] = positionsCopy[playerIndex] - 10
        end
        local r1, r2 = play(positionsCopy, scoresCopy, rollsLeftInTurn - 1, isPlayer1sTurn, memo)
        wins1 = wins1 + r1
        wins2 = wins2 + r2
    end

    memo[key] = {wins1, wins2}
    return wins1, wins2
end

function parseInput(input)
    local ans = {}
    for line in input:gmatch("[^\n]+") do
        local player, startingPosition = line:match("Player (%d) starting position: (%d)")
        table.insert(ans, tonumber(startingPosition))
    end
    return ans
end

local result = solve(input)
print(result)
