
function readIPRanges(filename)
    ranges = []
    open(filename) do file
        for line in eachline(file)
            parts = split(line, "-")
            start = parse(UInt32, parts[1])
            stop = parse(UInt32, parts[2])
            push!(ranges, (start, stop))
        end
    end
    return ranges
end

function findUnblockedIP(ranges)
    currentIP = 0
    for r in sort(ranges)
        if r[1] > currentIP
            return currentIP
        end
        if r[2] >= currentIP
            currentIP = r[2] + 1
        end
    end
    return currentIP
end

ipRanges = readIPRanges("input.txt")
unblockedIP = findUnblockedIP(ipRanges)
println(unblockedIP)
