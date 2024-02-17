
using DelimitedFiles

startingNumbers = readdlm("input.txt", ',', Int)

lastSpoken = Dict{Int, Int}()
lastNumber = 0
nextNumber = 0

for turn in 1:2020
    if turn <= length(startingNumbers)
        lastNumber = startingNumbers[turn]
        lastSpoken[lastNumber] = turn
        continue
    end
    
    if haskey(lastSpoken, lastNumber) && lastSpoken[lastNumber] != turn - 1
        nextNumber = turn - 1 - lastSpoken[lastNumber]
    else
        nextNumber = 0
    end
    
    lastSpoken[lastNumber] = turn - 1
    global lastNumber = nextNumber  # Fixing the warning by specifying global scope
end

println(lastNumber)
