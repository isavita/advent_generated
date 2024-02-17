
function readBusIDs(fileName)
    file = open(fileName)
    lines = readlines(file)
    close(file)
    
    busData = split(lines[2], ",")
    
    ids = Int[]
    offsets = Int[]
    for (i, bus) in enumerate(busData)
        if bus != "x"
            id = parse(Int, bus)
            push!(ids, id)
            push!(offsets, i-1)
        end
    end
    
    return ids, offsets
end

function extendedGCD(a, b)
    if a == 0
        return 0, 1
    end
    x1, y1 = extendedGCD(b % a, a)
    x = y1 - div(b, a) * x1
    y = x1
    return x, y
end

function findEarliestTimestamp(ids, offsets)
    N = 1
    for id in ids
        N *= id
    end
    
    result = 0
    for (i, id) in enumerate(ids)
        ni = div(N, id)
        xi, _ = extendedGCD(ni, id)
        result += (-offsets[i] + id) % id * xi * ni
    end
    
    return result % N
end

ids, offsets = readBusIDs("input.txt")
timestamp = findEarliestTimestamp(ids, offsets)
println(timestamp)
