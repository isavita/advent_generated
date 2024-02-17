
function parseInput(input)
    histories = []
    for line in input
        numbers = parseStringToInts(line)
        push!(histories, numbers)
    end
    return histories
end

function parseStringToInts(numbersLine)
    numbers = []
    numbersParts = split(numbersLine)
    for numberStr in numbersParts
        number = parse(Int, numberStr)
        push!(numbers, number)
    end
    return numbers
end

function allZeros(nums)
    for num in nums
        if num != 0
            return false
        end
    end
    return true
end

function calculateExtrapolation(history)
    extrapolations = []
    for i in 2:length(history)
        extrapolation = history[i] - history[i-1]
        push!(extrapolations, extrapolation)
    end
    return extrapolations
end

function calculateExtrapolations(history)
    extrapolationsSeries = []
    push!(extrapolationsSeries, history)

    for i in 2:length(history)
        previousExtrapolations = extrapolationsSeries[i-1]
        if allZeros(previousExtrapolations)
            return extrapolationsSeries
        end

        extrapolations = calculateExtrapolation(previousExtrapolations)
        push!(extrapolationsSeries, extrapolations)
    end

    return extrapolationsSeries
end

function solve(input)
    histories = parseInput(input)
    res = 0

    for history in histories
        extrapolationsSeries = calculateExtrapolations(history)

        pastPrediction = 0
        for i in length(extrapolationsSeries):-1:1
            pastPrediction = extrapolationsSeries[i][1] - pastPrediction
        end

        res += pastPrediction
    end

    return res
end

function readFile(fileName)
    file = read(fileName, String)
    return split(strip(file), '\n')
end

input = readFile("input.txt")
println(solve(input))
