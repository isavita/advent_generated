
adapters = [0]
for line in eachline("input.txt")
    push!(adapters, parse(Int, line))
end

sort!(adapters)
push!(adapters, adapters[end] + 3)

function countArrangements(adapters)
    ways = Dict(0 => 1)
    
    for i in 2:length(adapters)
        currentJoltage = adapters[i]
        for diff in [1, 2, 3]
            ways[currentJoltage] = get(ways, currentJoltage, 0) + get(ways, currentJoltage - diff, 0)
        end
    end
    
    return ways[adapters[end]]
end

println(countArrangements(adapters))
