using DelimitedFiles

function readInput(filename)
    totalElves = parse(Int, readlines(filename)[1])
    return totalElves
end

function findWinningElf(totalElves)
    highestPowerOfTwo = 1
    while highestPowerOfTwo * 2 <= totalElves
        highestPowerOfTwo *= 2
    end
    return (totalElves - highestPowerOfTwo) * 2 + 1
end

totalElves = readInput("input.txt")
winner = findWinningElf(totalElves)
println(winner)