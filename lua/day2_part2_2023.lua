local file = io.open("input.txt", "r")
if not file then
    print("Error opening file")
    return
end

local totalPower = 0
for line in file:lines() do
    local gameId, gameData = string.match(line, "Game (%d+): (.+)")
    if gameId and gameData then
        local rounds = {}
        for round in string.gmatch(gameData, "[^;]+") do
            table.insert(rounds, round)
        end

        local maxRed, maxGreen, maxBlue = 0, 0, 0
        for _, round in ipairs(rounds) do
            local red, green, blue = 0, 0, 0
            for count, color in string.gmatch(round, "(%d+) (%a+)") do
                count = tonumber(count)
                if color == "red" then
                    red = red + count
                elseif color == "green" then
                    green = green + count
                elseif color == "blue" then
                    blue = blue + count
                end
            end

            if red > maxRed then maxRed = red end
            if green > maxGreen then maxGreen = green end
            if blue > maxBlue then maxBlue = blue end
        end

        local power = maxRed * maxGreen * maxBlue
        totalPower = totalPower + power
    end
end

file:close()
print(totalPower)