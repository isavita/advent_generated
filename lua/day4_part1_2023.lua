local function parseCard(line)
    local _, _, winning, have = line:find(":(.*)%|(.*)")
    local winningNumbers = {}
    for num in winning:gmatch("%d+") do
        winningNumbers[tonumber(num)] = true
    end

    local count = 0
    for num in have:gmatch("%d+") do
        if winningNumbers[tonumber(num)] then
            count = count + 1
        end
    end

    if count > 0 then
        return 2 ^ (count - 1)
    else
        return 0
    end
end

local totalPoints = 0
for line in io.lines("input.txt") do
    totalPoints = totalPoints + parseCard(line)
end

print(totalPoints)
