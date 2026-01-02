
using Base.Iterators: partition

function contains(v, merged)
    l = 1
    r = length(merged) + 1
    while l < r
        m = (l + r) >>> 1
        lo, hi = merged[m]
        if v < lo
            r = m
        elseif v > hi
            l = m + 1
        else
            return true
        end
    end
    false
end

function main()
    lines = readlines("input.txt")
    parsing = true
    ranges = Tuple{BigInt,BigInt}[]
    ids = BigInt[]
    for line in lines
        s = strip(line)
        isempty(s) && (parsing = false; continue)
        parsing ? begin
            a, b = split(s, '-')
            push!(ranges, (parse(BigInt, a), parse(BigInt, b)))
        end : push!(ids, parse(BigInt, s))
    end

    sort!(ranges, by = first)

    merged = Tuple{BigInt,BigInt}[]
    for (mn, mx) in ranges
        if isempty(merged) || mn > merged[end][2]
            push!(merged, (mn, mx))
        elseif mx > merged[end][2]
            merged[end] = (merged[end][1], mx)
        end
    end

    fresh = 0
    for id in ids
        fresh += contains(id, merged) ? 1 : 0
    end

    println("Number of fresh ingredients: $fresh")
end

main()
