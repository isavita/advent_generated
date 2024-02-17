
data = readlines("input.txt")
frequencyChanges = map(x -> parse(Int, x), data)
frequencies = Dict{Int, Bool}()
currentFrequency = 0
frequencies[currentFrequency] = true

while true
    for change in frequencyChanges
        global currentFrequency
        currentFrequency += change
        if haskey(frequencies, currentFrequency)
            println(currentFrequency)
            return
        end
        frequencies[currentFrequency] = true
    end
end
