
local file = io.open("input.txt", "r")
if not file then
    print("Error opening file")
    return
end

local totalScore = 0

for line in file:lines() do
    local opponent = string.sub(line, 1, 1)
    local yourMove = string.sub(line, 3, 3)

    local score = 0
    if yourMove == 'X' then
        score = 1
    elseif yourMove == 'Y' then
        score = 2
    elseif yourMove == 'Z' then
        score = 3
    end

    if (opponent == 'A' and yourMove == 'Y') or (opponent == 'B' and yourMove == 'Z') or (opponent == 'C' and yourMove == 'X') then
        score = score + 6
    elseif (opponent == 'A' and yourMove == 'X') or (opponent == 'B' and yourMove == 'Y') or (opponent == 'C' and yourMove == 'Z') then
        score = score + 3
    end

    totalScore = totalScore + score
end

file:close()

print(totalScore)
