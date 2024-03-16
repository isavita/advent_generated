# Read input from "input.txt"
data = read("input.txt", String)
startingNumbers = parse.(Int, split(strip(data), ","))

spoken = Dict{Int, Int}()

global lastSpoken = 0
for (i, number) in enumerate(startingNumbers)
    if i == length(startingNumbers)
        global lastSpoken = number
    else
        spoken[number] = i
    end
end

for turn in length(startingNumbers) + 1:30_000_000
    global nextNumber = 0
    if haskey(spoken, lastSpoken)
        nextNumber = turn - 1 - spoken[lastSpoken]
    end
    spoken[lastSpoken] = turn - 1
    global lastSpoken = nextNumber
end

println(lastSpoken)