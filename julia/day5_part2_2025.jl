
function main()
    lines = filter(!isempty, map(strip, readlines("input.txt")))
    ranges = Tuple{BigInt,BigInt}[]
    for line in lines
        parts = split(line, '-')
        length(parts) == 2 || continue
        try
            a = parse(BigInt, parts[1])
            b = parse(BigInt, parts[2])
            a > b && ((a, b) = (b, a))
            push!(ranges, (a, b))
        catch
        end
    end
    isempty(ranges) && (println("Total fresh IDs: 0"); return)
    sort!(ranges, by = first)
    total = big(0)
    curMin, curMax = ranges[1]
    for (mn, mx) in Iterators.drop(ranges, 1)
        if mn <= curMax
            mx > curMax && (curMax = mx)
        else
            total += curMax - curMin + 1
            curMin, curMax = mn, mx
        end
    end
    total += curMax - curMin + 1
    println("Total fresh IDs: $total")
end

main()
