
function parseInput(input)
    local histories = {}
    for _, line in ipairs(input) do
        local numbers = parseStringToInts(line)
        table.insert(histories, numbers)
    end
    return histories
end

function parseStringToInts(numbersLine)
    local numbers = {}
    for numberStr in string.gmatch(numbersLine, "%S+") do
        local number = tonumber(numberStr)
        table.insert(numbers, number)
    end
    return numbers
end

function allZeros(nums)
    for _, num in ipairs(nums) do
        if num ~= 0 then
            return false
        end
    end
    return true
end

function calculateExtrapolation(history)
    local extrapolations = {}
    for i = 2, #history do
        local extrapolation = history[i] - history[i-1]
        table.insert(extrapolations, extrapolation)
    end
    return extrapolations
end

function calculateExtrapolations(history)
    local extrapolationsSeries = {}
    table.insert(extrapolationsSeries, history)

    for i = 2, #history do
        local previousExtrapolations = extrapolationsSeries[#extrapolationsSeries]
        if allZeros(previousExtrapolations) then
            return extrapolationsSeries
        end

        local extrapolations = calculateExtrapolation(previousExtrapolations)
        table.insert(extrapolationsSeries, extrapolations)
    end

    return extrapolationsSeries
end

function solve(input)
    local histories = parseInput(input)
    local res = 0

    for _, history in ipairs(histories) do
        local extrapolationsSeries = calculateExtrapolations(history)

        local futurePrediction = 0
        for i = #extrapolationsSeries, 1, -1 do
            futurePrediction = extrapolationsSeries[i][#extrapolationsSeries[i]] + futurePrediction
        end

        res = res + futurePrediction
    end

    return res
end

function readFile(fileName)
    local file = io.open(fileName, "r")
    local content = file:read("*all")
    file:close()
    return content
end

local input = {}
for line in io.lines("input.txt") do
    table.insert(input, line)
end

print(solve(input))
