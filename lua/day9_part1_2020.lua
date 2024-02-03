
local file = io.open("input.txt", "r")
local numbers = {}

for line in file:lines() do
    table.insert(numbers, tonumber(line))
end

local preambleLength = 25

local function isValid(number, previousNumbers)
    local seen = {}
    for _, n in ipairs(previousNumbers) do
        if seen[number - n] then
            return true
        end
        seen[n] = true
    end
    return false
end

for i = preambleLength + 1, #numbers do
    local currentNumber = numbers[i]
    local previousNumbers = {}
    for j = i - preambleLength, i - 1 do
        table.insert(previousNumbers, numbers[j])
    end
    if not isValid(currentNumber, previousNumbers) then
        print(currentNumber)
        break
    end
end

file:close()
