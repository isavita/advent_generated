function find_combinations(containers, target, index, count, mincount, ways)
    if target == 0
        if mincount[1] == 0 || count < mincount[1]
            mincount[1] = count
            ways[1] = 1
        elseif count == mincount[1]
            ways[1] += 1
        end
        return
    end
    if target < 0 || index >= length(containers)
        return
    end
    # Include current container
    find_combinations(containers, target-containers[index+1], index+1, count+1, mincount, ways)
    # Exclude current container
    find_combinations(containers, target, index+1, count, mincount, ways)
end

file = open("input.txt", "r")
containers = []
for line in eachline(file)
    push!(containers, parse(Int, line))
end
close(file)

mincount = [0]
ways = [0]
find_combinations(containers, 150, 0, 0, mincount, ways)
println(ways[1])