local file = io.open("input.txt", "r")
if not file then
    print("Error opening file")
    return
end

local totalSum = 0

for line in file:lines() do
    local gameId, rounds = string.match(line, "Game (%d+): (.+)")
    if gameId then
        gameId = tonumber(gameId)
        local isValid = true
        local red, green, blue = 0, 0, 0
        for round in string.gmatch(rounds, "[^;]+") do
            red, green, blue = 0, 0, 0
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
            if red > 12 or green > 13 or blue > 14 then
                isValid = false
                break
            end
        end
        if isValid then
            totalSum = totalSum + gameId
        end
    end
end

file:close()
print(totalSum)