
function readNodes(filename)
    nodes = []
    for line in eachline(filename)
        m = match(r"node-x\d+-y\d+\s+\d+T\s+(\d+)T\s+(\d+)T\s+\d+%", line)
        if m !== nothing
            used = parse(Int, m.captures[1])
            avail = parse(Int, m.captures[2])
            push!(nodes, (used, avail))
        end
    end
    return nodes
end

function countViablePairs(nodes)
    count = 0
    for i in 1:length(nodes)
        for j in 1:length(nodes)
            if i != j && nodes[i][1] > 0 && nodes[i][1] <= nodes[j][2]
                count += 1
            end
        end
    end
    return count
end

nodes = readNodes("input.txt")
viablePairs = countViablePairs(nodes)
println(viablePairs)
