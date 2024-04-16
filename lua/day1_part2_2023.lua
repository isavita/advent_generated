function findFirstAndLastDigit(line)
    local digits = {"zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"}
    local firstDigit, lastDigit = nil, nil

    for i = 1, #line do
        local char = line:sub(i, i)
        local digitStr = tonumber(char)
        if digitStr then
            if not firstDigit then
                firstDigit = digitStr
            end
            lastDigit = digitStr
        else
            for j, digit in ipairs(digits) do
                if line:sub(i, i + #digit - 1) == digit then
                    if not firstDigit then
                        firstDigit = j - 1
                    end
                    lastDigit = j - 1
                    break
                end
            end
        end
    end

    return firstDigit, lastDigit
end

local sum = 0
local file = io.open("input.txt", "r")
for line in file:lines() do
    local firstDigit, lastDigit = findFirstAndLastDigit(line)
    sum = sum + 10 * firstDigit + lastDigit
end
file:close()

print(sum)